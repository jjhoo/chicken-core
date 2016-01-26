;;;; load.scm - Code loading for CHICKEN
;
; Copyright (c) 2008-2015, The CHICKEN Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

(declare
  (unit load)
  (uses eval internal))

#>

#ifndef C_INSTALL_EGG_HOME
# define C_INSTALL_EGG_HOME    "."
#endif

#ifndef C_INSTALL_SHARE_HOME
# define C_INSTALL_SHARE_HOME NULL
#endif

#ifndef C_BINARY_VERSION
# define C_BINARY_VERSION      0
#endif

<#

(include "common-declarations.scm")

(module chicken.load
  (load load-library load-noisily load-relative load-verbose
   extension-information chicken-home dynamic-load-libraries
   require repository-path set-dynamic-load-mode!)

(import (except scheme eval load))

(import chicken
	chicken.eval
	chicken.foreign
	chicken.internal)

(define-foreign-variable install-egg-home c-string "C_INSTALL_EGG_HOME")
(define-foreign-variable installation-home c-string "C_INSTALL_SHARE_HOME")
(define-foreign-variable binary-version int "C_BINARY_VERSION")
(define-foreign-variable uses-soname? bool "C_USES_SONAME")
(define-foreign-variable install-lib-name c-string "C_INSTALL_LIB_NAME")

;; TODO take these mappings from import files instead
(define-constant core-chicken-modules
  '((chicken . chicken-syntax)
    (chicken.bitwise . library)
    (chicken.continuation . continuation)
    (chicken.data-structures . data-structures)
    (chicken.eval . eval)
    (chicken.expand . expand)
    (chicken.files . files)
    (chicken.flonum . library)
    (chicken.foreign . chicken-ffi-syntax)
    (chicken.format . extras)
    (chicken.gc . library)
    (chicken.internal . internal)
    (chicken.io . extras)
    (chicken.irregex . irregex)
    (chicken.keyword . library)
    (chicken.locative . lolevel)
    (chicken.load . load)
    (chicken.lolevel . lolevel)
    (chicken.ports . ports)
    (chicken.posix . posix)
    (chicken.pretty-print . extras)
    (chicken.tcp . tcp)
    (chicken.time . library)
    (chicken.repl . repl)
    (chicken.read-syntax . read-syntax)
    (chicken.utils . utils)))

(define-constant core-library-units
  `(srfi-4 . ,(map cdr core-chicken-modules)))

(define-constant core-syntax-units
  '(chicken-syntax chicken-ffi-syntax))

(define-constant cygwin-default-dynamic-load-libraries '("cygchicken-0"))
(define-constant macosx-load-library-extension ".dylib")
(define-constant windows-load-library-extension ".dll")
(define-constant hppa-load-library-extension ".sl")
(define-constant default-load-library-extension ".so")
(define-constant environment-table-size 301)
(define-constant source-file-extension ".scm")
(define-constant setup-file-extension "setup-info")
(define-constant repository-environment-variable "CHICKEN_REPOSITORY")
(define-constant prefix-environment-variable "CHICKEN_PREFIX")

; these are actually in unit extras, but that is used by default
; srfi-12 in unit library
; srfi-98 partially in unit posix

(define-constant builtin-features
  '(scheme chicken
    srfi-2 srfi-6 srfi-10 srfi-12
    srfi-23 srfi-28 srfi-30 srfi-39
    srfi-55 srfi-88 srfi-98))

(define-constant builtin-features/compiled
  '(srfi-8 srfi-9 srfi-11 srfi-15 srfi-16 srfi-17 srfi-26) )

(define default-dynamic-load-libraries
  (case (build-platform)
    ((cygwin) cygwin-default-dynamic-load-libraries)
    (else `(,(string-append "lib" install-lib-name)))))

(define chicken-prefix
  (let ((prefix (and-let* ((p (get-environment-variable prefix-environment-variable)))
		  (##sys#string-append
		   p
		   (if (memq (string-ref p (fx- (##sys#size p) 1)) '(#\\ #\/)) "" "/")) ) ) )
    (lambda (#!optional dir)
      (and prefix
	   (if dir (##sys#string-append prefix dir) prefix) ) ) ) )

;;; System settings

(define (chicken-home)
  (or (chicken-prefix "share/chicken") installation-home))


;;; Loading source/object files:

(define load-verbose (make-parameter (##sys#fudge 13)))

(define ##sys#current-source-filename #f)
(define ##sys#current-load-path "")
(define ##sys#dload-disabled #f)

(define-foreign-variable _dlerror c-string "C_dlerror")

(define (set-dynamic-load-mode! mode)
  (let ([mode (if (pair? mode) mode (list mode))]
	[now #f]
	[global #t] )
    (let loop ([mode mode])
      (when (pair? mode)
	(case (##sys#slot mode 0)
	  [(global) (set! global #t)]
	  [(local) (set! global #f)]
	  [(lazy) (set! now #f)]
	  [(now) (set! now #t)]
	  [else (##sys#signal-hook 'set-dynamic-load-mode! "invalid dynamic-load mode" (##sys#slot mode 0))] )
	(loop (##sys#slot mode 1)) ) )
    (##sys#set-dlopen-flags! now global) ) )

(define (toplevel name)
  (if (not name)
      "toplevel"
      (##sys#string-append
       (string->c-identifier (##sys#slot name 1))
       "_toplevel")))

(define (c-toplevel name loc)
  (##sys#make-c-string (##sys#string-append "C_" (toplevel name)) loc))

(define load/internal
  (let ((read read)
	(write write)
	(display display)
	(newline newline)
	(eval eval)
	(open-input-file open-input-file)
	(close-input-port close-input-port))
    (lambda (input evaluator #!optional pf timer printer unit)

      (define evalproc
	(or evaluator eval))

      (define (has-slash? str)
	(let loop ((i (fx- (##sys#size str) 1)))
	  (if (memq (##core#inline "C_subchar" str i) '(#\\ #\/))
	      i
	      (and (fx< 0 i)
		   (loop (fx- i 1))))))

      ;; dload doesn't consider filenames without slashes to be paths,
      ;; so we prepend a dot to force a relative pathname.
      (define (dload-path path)
	(if (has-slash? path)
	    path
	    (##sys#string-append "./" path)))

      (define (dload path)
	(let ((c-path (##sys#make-c-string (dload-path path) 'load)))
	  (or (##sys#dload c-path (c-toplevel unit 'load))
	      (and (symbol? unit)
		   (##sys#dload c-path (c-toplevel #f 'load))))))

      (define dload?
	(and (not ##sys#dload-disabled)
	     (##sys#fudge 24)))

      (define fname
	(cond ((port? input) #f)
	      ((not (string? input))
	       (##sys#signal-hook #:type-error 'load "bad argument type - not a port or string" input))
	      ((##sys#file-exists? input #t #f 'load) input)
	      (else
	       (let ((fname2 (##sys#string-append input ##sys#load-dynamic-extension)))
		 (if (and dload? (##sys#file-exists? fname2 #t #f 'load))
		     fname2
		     (let ((fname3 (##sys#string-append input source-file-extension)))
		       (if (##sys#file-exists? fname3 #t #f 'load)
			   fname3
			   input)))))))

      (when (and (string? input) (not fname))
	(##sys#signal-hook #:file-error 'load "cannot open file" input))

      (when (and (load-verbose) fname)
	(display "; loading ")
	(display fname)
	(display " ...\n")
	(flush-output))

      (or (and fname dload? (dload fname))
	  (call-with-current-continuation
	   (lambda (abrt)
	     (fluid-let ((##sys#read-error-with-line-number #t)
			 (##sys#current-source-filename fname)
			 (##sys#current-load-path
			  (and fname
			       (let ((i (has-slash? fname)))
				 (if i (##sys#substring fname 0 (fx+ i 1)) "")))))
	       (let ((in (if fname (open-input-file fname) input)))
		 (##sys#dynamic-wind
		  (lambda () #f)
		  (lambda ()
		    (let ((c1 (peek-char in)))
		      (when (eq? c1 (integer->char 127))
			(##sys#error
			 'load
			 (##sys#string-append
			  "unable to load compiled module - "
			  (or _dlerror "unknown reason"))
			 fname)))
		    (let ((x1 (read in)))
		      (do ((x x1 (read in)))
			  ((eof-object? x))
			(when printer (printer x))
			(##sys#call-with-values
			 (lambda ()
			   (if timer
			       (time (evalproc x))
			       (evalproc x)))
			 (lambda results
			   (when pf
			     (for-each
			      (lambda (r)
				(write r)
				(newline))
			      results)))))))
		  (lambda ()
		    (close-input-port in))))))))
      (##core#undefined))))

(define (load filename . evaluator)
  (load/internal filename (optional evaluator #f)))

(define (load-relative filename . evaluator)
  (load/internal
   (if (memq (string-ref filename 0) '(#\\ #\/))
       filename
       (##sys#string-append ##sys#current-load-path filename))
   (optional evaluator #f)))

(define (load-noisily filename #!key (evaluator #f) (time #f) (printer #f))
  (load/internal filename evaluator #t time printer))

(define load-library-extension ; this is crude...
  (cond [(eq? (software-type) 'windows) windows-load-library-extension]
	[(eq? (software-version) 'macosx) macosx-load-library-extension]
	[(and (eq? (software-version) 'hpux) 
	      (eq? (machine-type) 'hppa)) hppa-load-library-extension]
	[else default-load-library-extension] ) )

(define ##sys#load-dynamic-extension default-load-library-extension)

(define dynamic-load-libraries 
  (let ((ext
	 (if uses-soname?
	     (string-append
	      load-library-extension
	      "." 
	      (number->string binary-version))
	     load-library-extension)))
    (define complete
      (cut ##sys#string-append <> ext))
    (make-parameter
     (map complete default-dynamic-load-libraries)
     (lambda (x)
       (##sys#check-list x)
       x) ) ) )

(define load-library-0
  (let ((display display))
    (lambda (uname lib)
      (or (##sys#provided? uname)
	  (let ((libs
		 (if lib
		     (##sys#list lib)
		     (cons (##sys#string-append (##sys#slot uname 1) load-library-extension)
			   (dynamic-load-libraries))))
		(top
		 (c-toplevel uname 'load-library)))
	    (when (load-verbose)
	      (display "; loading library ")
	      (display uname)
	      (display " ...\n") )
	    (let loop ((libs libs))
	      (cond ((null? libs) #f)
		    ((##sys#dload (##sys#make-c-string (##sys#slot libs 0) 'load-library) top)
		     (##sys#provide uname))
		    (else (loop (##sys#slot libs 1))))))))))

(define load-library
  (lambda (uname #!optional lib)
    (##sys#check-symbol uname 'load-library)
    (unless (not lib) (##sys#check-string lib 'load-library))
    (or (load-library-0 uname lib)
	(##sys#error 'load-library "unable to load library" uname _dlerror) ) ) )

(define ##sys#load-library load-library)

(define ##sys#include-forms-from-file
  (let ((with-input-from-file with-input-from-file)
	(read read)
	(reverse reverse))
    (lambda (fname)
      (let ((path (##sys#resolve-include-filename fname #t)))
	(when (load-verbose) (print "; including " path " ..."))
	(with-input-from-file path
	  (lambda ()
	    (fluid-let ((##sys#current-source-filename path))
	      (do ((x (read) (read))
		   (xs '() (cons x xs)) )
		  ((eof-object? x)
		   (reverse xs))) ) ) ) ) ) ) )


;;; Extensions:

(define ##sys#canonicalize-extension-path
  (let ([string-append string-append])
    (lambda (id loc)
      (define (err) (##sys#error loc "invalid extension path" id))
      (define (sep? c) (or (char=? #\\ c) (char=? #\/ c)))
      (let ([p (cond [(string? id) id]
		     [(symbol? id) (##sys#symbol->string id)]
		     [(list? id)
		      (let loop ([id id])
			(if (null? id)
			    ""
			    (string-append
			     (let ([id0 (##sys#slot id 0)])
			       (cond [(symbol? id0) (##sys#symbol->string id0)]
				     [(string? id0) id0]
				     [else (err)] ) )
			     (if (null? (##sys#slot id 1))
				 ""
				 "/")
			     (loop (##sys#slot id 1)) ) ) ) ] ) ] )
	(let check ([p p])
	  (let ([n (##sys#size p)])
	    (cond [(fx= 0 n) (err)]
		  [(sep? (string-ref p 0))
		   (check (##sys#substring p 1 n)) ]
		  [(sep? (string-ref p (fx- n 1)))
		   (check (##sys#substring p 0 (fx- n 1))) ]
		  [else p] ) ) ) ) ) ) )

(define repository-path
  (let ((rpath
	 (if (##sys#fudge 22)		; private repository?
	     (foreign-value "C_private_repository_path()" c-string)
	     (or (get-environment-variable repository-environment-variable)
		 (chicken-prefix
		  (##sys#string-append
		   "lib/chicken/"
		   (##sys#number->string (##sys#fudge 42))) )
		 install-egg-home))))
    (lambda (#!optional val)
      (if val
	  (set! rpath val)
	  rpath))))

(define ##sys#repository-path repository-path)

(define ##sys#setup-mode #f)

(define ##sys#find-extension
  (let ((file-exists? file-exists?)
	(string-append string-append))
    (lambda (p inc?)
      (let ((rp (##sys#repository-path)))
	(define (check path)
	  (let ((p0 (string-append path "/" p)))
	    (or (and rp
		     (not ##sys#dload-disabled)
		     (##sys#fudge 24) ; dload?
		     (file-exists? (##sys#string-append p0 ##sys#load-dynamic-extension)))
		(file-exists? (##sys#string-append p0 source-file-extension)))))
	(let loop ((paths (##sys#append
			   (if ##sys#setup-mode '(".") '())
			   (if rp (list rp) '())
			   (if inc? ##sys#include-pathnames '())
			   (if ##sys#setup-mode '() '("."))) ))
	  (and (pair? paths)
	       (let ((pa (##sys#slot paths 0)))
		 (or (check pa)
		     (loop (##sys#slot paths 1)) ) ) ) ) ) ) ))

(define (load-extension id)
  (define (fail message)
    (##sys#error 'require message id))
  (cond ((string? id) (set! id (string->symbol id)))
	(else (##sys#check-symbol id 'require)))
  (cond ((##sys#provided? id))
	((memq id core-syntax-units)
	 (fail "cannot load core library"))
	((memq id core-library-units)
	 (or (load-library-0 id #f)
	     (fail "cannot load core library")))
	(else
	 (let* ((path (##sys#canonicalize-extension-path id 'require))
		(ext  (##sys#find-extension path #f)))
	   (cond (ext
		  (load/internal ext #f #f #f #f id)
		  (##sys#provide id))
		 (else
		  (fail "cannot load extension")))))))

(define (require . ids)
  (for-each load-extension ids))

(define ##sys#require require)

(define extension-information/internal
  (let ([with-input-from-file with-input-from-file]
	[string-append string-append]
	[read read] )
    (lambda (id loc)
      (and-let* ((rp (##sys#repository-path)))
	(let* ((p (##sys#canonicalize-extension-path id loc))
	       (rpath (string-append rp "/" p ".")) )
	  (cond ((file-exists? (string-append rpath setup-file-extension))
		 => (cut with-input-from-file <> read) )
		(else #f) ) ) ) ) ))

(define (extension-information ext)
  (extension-information/internal ext 'extension-information))

;; FIXME This is qualified for visibility in the eval module until
;; module-namespaced identifiers are allowed to be unbound.
(define ##sys#lookup-runtime-requirements
  (let ([with-input-from-file with-input-from-file]
	[read read] )
    (lambda (ids)
      (let loop1 ([ids ids])
	(if (null? ids)
	    '()
	    (append
	     (or (and-let* ((info (extension-information/internal (car ids) #f))
			    (a (assq 'require-at-runtime info)))
		   (cdr a) )
		 '() )
	     (loop1 (cdr ids)) ) ) ) ) ) )

;;
;; Given a library specification, returns three values:
;;
;;   - an expression for loading the library, if required
;;   - a fixed-up library id if the library was found, #f otherwise
;;   - a requirement type (e.g. 'dynamic) or #f if provided statically
;;
(define (##sys#expand-require lib #!optional (static-units '()))
  (let ((id (library-id lib))
	(compiling? (feature? #:compiling)))
    (cond
      ((assq id core-chicken-modules) =>
       (lambda (mod)
	 (##sys#expand-require (cdr mod) static-units)))
      ((or (memq id builtin-features)
	   (and compiling? (memq id builtin-features/compiled)))
       (values '(##core#undefined) id #f))
      ((memq id static-units)
       (values '(##core#undefined) id #f))
      ((and (not compiling?) (##sys#feature? id))
       (values '(##core#undefined) id #f))
      ((memq id core-syntax-units)
       (values '(##core#undefined) id #f))
      ((memq id core-library-units)
       (values
	(if compiling?
	    `(##core#declare (uses ,id))
	    `(##sys#load-library (##core#quote ,id) #f))
	id #f))
      ((extension-information/internal id 'require) =>
       (lambda (info)
	 (let ((s  (assq 'syntax info))
	       (nr (assq 'syntax-only info))
	       (rr (assq 'require-at-runtime info)))
	   (values
	    `(##core#begin
	      ,@(if s `((##core#require-for-syntax (##core#quote ,id))) '())
	      ,@(if (or nr (and (not rr) s))
		    '()
		    (begin
		      `((##sys#require
			 ,@(map (lambda (id) `(##core#quote ,id))
				(cond (rr (cdr rr))
				      (else (list id)))))))))
	    id
	    (if s 'dynamic/syntax 'dynamic)))))
      (else
       (values `(##sys#require (##core#quote ,id)) #f 'dynamic)))))

;;; Find included file:

(define ##sys#include-pathnames
  (let ((h (chicken-home)))
    (if h (list h) '())) )

(define ##sys#resolve-include-filename
  (let ((string-append string-append) )
    (define (exists? fname)
      (##sys#file-exists? fname #t #f #f))
    (lambda (fname prefer-source #!optional repo)
      (define (test2 fname lst)
	(if (null? lst)
	    (and (exists? fname) fname)
	    (let ([fn (##sys#string-append fname (car lst))])
	      (if (exists? fn)
		  fn
		  (test2 fname (cdr lst)) ) ) ) )
      (define (test fname)
	(test2
	 fname
	 (cond ((not (##sys#fudge 24)) (list source-file-extension)) ; no dload?
	       (prefer-source (list source-file-extension ##sys#load-dynamic-extension))
	       (else (list ##sys#load-dynamic-extension source-file-extension) ) ) ))
      (or (test fname)
	  (let loop ((paths (if repo
				(##sys#append
				 ##sys#include-pathnames
				 (let ((rp (##sys#repository-path)))
				   (if rp
				       (list (##sys#repository-path))
				       '())))
				##sys#include-pathnames) ) )
	    (cond ((eq? paths '()) fname)
		  ((test (string-append (##sys#slot paths 0)
					"/"
					fname) ) )
		  (else (loop (##sys#slot paths 1))) ) ) ) ) ) )

) ; load module

(declare (hide CHICKEN_load))

(define-external (CHICKEN_load (c-string str)) bool
  (chicken.internal#run-safe (lambda () (chicken.load#load str) #t)))
