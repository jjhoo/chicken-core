;;;;

(module main ()

(import (scheme))
(import (chicken))
(import (chicken foreign))
(import (chicken data-structures))
(import (chicken keyword))
(import (chicken files))
(import (chicken format))
(import (chicken irregex))
(import (chicken tcp))
(import (chicken posix))
(import (chicken io))
(import (chicken time))
(import (chicken pretty-print))

(define +defaults-version+ 1)
(define +module-db+ "modules.db")
(define +defaults-file+ "setup.defaults")
(define +short-options+ '(#\r #\h))
(define +one-hour+ (* 60 60))
(define +timestamp-file+ "TIMESTAMP")
(define +status-file+ "STATUS")
(define +egg-extension+ "egg")

(include "mini-srfi-1.scm")
(include "egg-environment.scm")
(include "egg-compile.scm")
(include "egg-download.scm")

(define quiet #f)
(define default-servers '())
(define default-locations '())
(define mappings '())
(define aliases '())
(define override '())
(define hacks '())
(define proxy-host #f)
(define proxy-port #f)
(define proxy-user-pass #f)
(define retrieve-only #f)
(define canonical-eggs '())
(define run-tests #f)
  
(define platform
  (if (eq? 'mingw (build-platform))
      'windows
      'unix))

(define current-status 
  (list (get-environment-variable "CSC_OPTIONS")))      ;XXX more?

(define (probe-dir dir)
  (and dir (file-exists? dir) (directory? dir) dir))
  
(define cache-directory
  (make-pathname (or (probe-dir (get-environment-variable "HOME"))
                     (probe-dir (get-environment-variable "USERPROFILE"))
                     (probe-dir "/tmp")
                     (probe-dir "/Temp")
                     ".")
                 ".chicken-install.cache"))
  
  
;; usage information
  
(define (usage code)
  ;;XXX  
  (exit code))
  

;; utilities

;; Simpler replacement for SRFI-13's string-suffix?
(define (string-suffix? suffix s)
  (let ((len-s (string-length s))
        (len-suffix (string-length suffix)))
     (and (not (< len-s len-suffix))
          (string=? suffix
   	            (substring s (- len-s len-suffix))))))

(define (d fstr . args)
  (let ((port (if quiet (current-error-port) (current-output-port))))
    (apply fprintf port fstr args)
    (flush-output port) ) )

(define (version>=? v1 v2)
  (define (version->list v)
    (map (lambda (x) (or (string->number x) x))
	 (irregex-split "[-\\._]" (->string v))))
  (let loop ((p1 (version->list v1))
	     (p2 (version->list v2)))
    (cond ((null? p1) (null? p2))
	  ((null? p2))
	  ((number? (car p1))
	   (and (number? (car p2))
		(or (> (car p1) (car p2))
		    (and (= (car p1) (car p2))
			 (loop (cdr p1) (cdr p2))))))
	  ((number? (car p2)))
	  ((string>? (car p1) (car p2)))
	  (else
	   (and (string=? (car p1) (car p2))
		(loop (cdr p1) (cdr p2)))))))


;; load defaults file ("setup.defaults")

(define (load-defaults)
  (let ((deff (make-pathname host-sharedir +defaults-file+)))
      (define (broken x)
	(error "invalid entry in defaults file" deff x))
      (cond ((not (file-exists? deff)) '())
            (else
	     (for-each
	      (lambda (x)
		(unless (and (list? x) (positive? (length x)))
		  (broken x))
		(case (car x)
		  ((version)
		   (cond ((not (pair? (cdr x))) (broken x))
			 ((not (= (cadr x) +defaults-version+))
			  (error 
			   (sprintf 
			       "version of installed `~a' does not match chicken-install version (~a)"
			     +defaults-file+
			     +defaults-version+)
			   (cadr x)))
			 ;; others are ignored
			 ))
		  ((server)
		   (set! default-servers
		     (append default-servers (list (cdr x)))))
		  ((map)
		   (set! mappings
		     (append
		      mappings
		      (map (lambda (m)
			     (let ((p (list-index (cut eq? '-> <>) m)))
			       (unless p (broken x))
			       (let-values (((from to) (split-at m p)))
				 (cons from (cdr to)))))
			   (cdr x)))))
		  ((alias)
		   (set! aliases
		     (append 
		      aliases
		      (map (lambda (a)
			     (if (and (list? a) (= 2 (length a)) (every string? a))
				 (cons (car a) (cadr a))
				 (broken x)))
			   (cdr x)))))
		  ((override)
		   (set! override
		     (if (and (pair? (cdr x)) (string? (cadr x)))
			 (call-with-input-file (cadr x) read-all)
			 (cdr x))))
                  ((location)
                   (set! default-locations
                     (append default-locations (list (cdr x)))))
		  ((hack)
		   (set! hacks (append hacks (list (eval (cadr x))))))
		  (else (broken x))))
	      (call-with-input-file deff read-all))))))

  
;; set variables with HTTP proxy information
  
(define (setup-proxy uri)
  (and-let* (((string? uri))
             (m (irregex-match "(http://)?([^:]+):?([0-9]*)" uri))
             (port (irregex-match-substring m 3)))
    (set! proxy-user-pass (get-environment-variable "proxy_auth"))
    (set! proxy-host (irregex-match-substring m 2))
    (set! proxy-port (or (string->number port) 80))))

  
;; apply egg->egg mappings loaded from defaults
  
(define (apply-mappings eggs)
  (define (canonical x)
    (cond ((symbol? x) (cons (symbol->string x) #f))
          ((string? x) (cons x #f))
          ((pair? x) x)
          (else (error "internal error - bad egg spec" x))))
  (define (same? e1 e2)
    (equal? (car (canonical e1)) (car (canonical e2))))
  (let ((eggs2
         (delete-duplicates
           (append-map
             (lambda (egg)
               (cond ((find (lambda (m) (find (cut same? egg <>) (car m)))
                        mappings) => 
                      (lambda (m) (map ->string (cdr m))))
                 (else (list egg))))
             eggs)
           same?)))
    (unless (and (= (length eggs) (length eggs2))
                 (every (lambda (egg) (find (cut same? <> egg) eggs2)) eggs))
      (d "mapped ~s to ~s~%" eggs eggs2))
    eggs2))

  
;; override versions, if specified in "overrides" file
  
(define (override-version egg)
  (let ((name (string->symbol (if (pair? egg) (car egg) egg))))
    (cond ((assq name override) =>
           (lambda (a)
             (cond ((and (pair? egg) (not (equal? (cadr a) (cdr egg))))
                    (warning
                      (sprintf 
                        "version `~a' of extension `~a' overrides explicitly given version `~a'"
                        (cadr a) name (cdr egg))))
                   (else (d "overriding: ~a~%" a)))
             (cadr a)))
          ((pair? egg) (cdr egg))
          (else #f))))
  
  
;; "locate" egg: either perform HTTP download or copy from a file-system 
;; location, also make sure it is up to date
  
(define (locate-egg name version)
  (let* ((cached (make-pathname cache-directory name))
         (now (current-seconds))
         (timestamp (make-pathname cached +timestamp-file+))
         (status (make-pathname cached +status-file+))
         (eggfile (make-pathname cached name +egg-extension+)))
    (define (fetch)
      (when (file-exists? cached)
        (delete-directory cached #t))
      (fetch-egg-sources name version cached)
      (with-output-to-file status (cut write current-status)))
    (cond ((not (probe-dir cached)) (fetch))
          ((and (file-exists? status)
                (not (equal? current-status 
                             (with-input-from-file status read))))
           (fetch)))
    (let* ((info (load-egg-info eggfile))
           (lversion (get-egg-property info 'version)))
      (cond ((and (file-exists? timestamp)
                  (> (- now (with-input-from-file timestamp read)) +one-hour+)
                  (not (check-server-version name version lversion)))
             (fetch)
             (let ((info (load-egg-info eggfile)))
               (values cached (get-egg-property info 'version))))
            (else (values cached version))))))
    
(define (fetch-egg-sources name version dest)
  (let loop ((locs default-locations))
    (cond ((null? locs)
           (let loop ((srvs default-servers))
             (receive (dir ver)
               (try-download name (car srvs) 
                             version: version 
                             destination: dest
                             tests: run-tests 
                             proxy-host: proxy-host
                             proxy-port: proxy-port 
                             proxy-user-pass: proxy-user-pass)
              (cond (dir
                     (with-output-to-file 
                       (make-pathname dest +timestamp-file+)
                       (lambda () (write (current-seconds)))))
                    ((null? srvs) (error "extension or version not found"))
                    (else (loop (cdr srvs)))))))
          ((probe-dir (make-pathname (car locs) name))
           => (lambda (dir)
                (let* ((eggfile (make-pathname dir name +egg-extension+))
                       (info (load-egg-info eggfile))
                       (rversion (get-egg-property info 'version)))
                  (if (or (not rversion)
                          (version>=? rversion version))
                      (copy-egg-sources dir dest)
                      (loop (cdr locs))))))
          (else (loop (cdr locs))))))
  
(define (copy-egg-sources from to)
  ;;XXX should probably be done manually, instead of calling tool
  (let ((cmd (quote-all
               (string-append
                 (copy-directory-command platform)
                 " " (quotearg from) " " (quotearg to))
               platform)))
    (system cmd)))
  
(define (check-server-version name version lversion)
  (let loop ((srvs default-servers))
    (and (pair? srvs)
         (let ((versions (try-list-versions name (car srvs))))
           (or (and versions
                    (any (cut version>=? <> version) versions))
               (loop (cdr srvs)))))))
   

;; retrieve eggs, recursively (if needed)
  
(define (retrieve-eggs eggs)
  (for-each
    (lambda (egg)
      (cond ((assoc egg canonical-eggs) =>
             (lambda (a)
               ;; push to front
               (set! canonical-eggs (cons a (delete a canonical-eggs eq?)))))
             (else
              (let ((name (if (pair? egg) (car egg) egg))
                    (version (override-version egg)))
                (let-values (((dir ver) (locate-egg name version)))
                  (when (or (not dir)
                            (null? (directory dir)))
                    (error "extension or version not found"))
                  (d " ~a located at ~a~%")
                  (set! canonical-eggs
                    (cons (list name dir ver) canonical-eggs)))))))
     eggs)
  (unless retrieve-only
    (error "to be implemented"))) ; XXX

  
;; command line parsing and selection of operations
  
(define (perform-actions eggs)
  (let ((eggs (apply-mappings eggs)))
    ;;XXX...
    (retrieve-eggs eggs)))

(define (main args)
  (setup-proxy (get-environment-variable "http_proxy"))
  (let ((eggs '())
        (rx (irregex "([^:]+):(.+)")))
    (let loop ((args args))
      (if (null? args)
          (perform-actions (reverse eggs))
          (let ((arg (car args)))
            (cond ((member arg '("-h" "-help" "--help"))
                   (usage 0))
                  ((equal? arg "-test")
                   (set! run-tests #t))

                  ;;XXX 
                  
                  ((and (positive? (string-length arg))
                        (char=? #\- (string-ref arg 0)))
                   (if (> (string-length arg) 2)
                       (let ((sos (string->list (substring arg 1))))
                         (if (every (cut memq <> +short-options+) sos)
                             (loop (append 
                                     (map (cut string #\- <>) sos)
                                     (cdr args)))
                             (usage 1)))
                       (usage 1)))
                  ((irregex-match rx arg) =>
                   (lambda (m)
                     (set! eggs
                       (alist-cons
                         (irregex-match-substring m 1)
                         (irregex-match-substring m 2)
                         eggs))))
                  (else 
                    (set! eggs (cons arg args))
                    (loop (cdr args)))))))))

(main (command-line-arguments))
  
)