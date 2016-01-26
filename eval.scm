;;;; eval.scm - Evaluation for CHICKEN
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
  (unit eval)
  (uses expand internal modules)
  (not inline ##sys#alias-global-hook ##sys#syntax-error-hook))

#>

#define C_rnd_fix()		(C_fix(rand()))

<#

(include "common-declarations.scm")

(module chicken.eval
  (eval eval-handler
   interaction-environment
   null-environment
   scheme-report-environment)

;; Exclude values defined within this module.
(import (except scheme eval interaction-environment null-environment scheme-report-environment))

(import chicken
	chicken.expand
	chicken.keyword)

(include "mini-srfi-1.scm")

(define-syntax d (syntax-rules () ((_ . _) (void))))

;;; Lo-level hashtable support:

(define ##sys#hash-symbol
  (let ([cache-s #f]
	[cache-h #f]
        ;; NOTE: All low-level hash tables share the same randomization factor
        [rand (##core#inline "C_rnd_fix")] )
    (lambda (s n)
      (if (eq? s cache-s)
	  (##core#inline "C_fixnum_modulo" cache-h n)
          (begin
            (set! cache-s s)
            (set! cache-h (##core#inline "C_u_i_string_hash" (##sys#slot s 1) rand))
            (##core#inline "C_fixnum_modulo" cache-h n))))))

(define (##sys#hash-table-ref ht key)
  (let loop ((bucket (##sys#slot ht (##sys#hash-symbol key (##core#inline "C_block_size" ht)))))
      (and (not (eq? '() bucket))
           (if (eq? key (##sys#slot (##sys#slot bucket 0) 0))
               (##sys#slot (##sys#slot bucket 0) 1)
               (loop (##sys#slot bucket 1))))))

(define (##sys#hash-table-set! ht key val)
  (let* ((k (##sys#hash-symbol key (##core#inline "C_block_size" ht)))
         (ib (##sys#slot ht k)))
      (let loop ((bucket ib))
          (if (eq? '() bucket)
              (##sys#setslot ht k (cons (cons key val) ib))
              (if (eq? key (##sys#slot (##sys#slot bucket 0) 0))
                  (##sys#setslot (##sys#slot bucket 0) 1 val)
                  (loop (##sys#slot bucket 1)))))))

(define (##sys#hash-table-update! ht key updtfunc valufunc)
  (##sys#hash-table-set! ht key (updtfunc (or (##sys#hash-table-ref ht key) (valufunc)))) )

(define (##sys#hash-table-for-each p ht)
  (let ((len (##core#inline "C_block_size" ht)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i len))
      (##sys#for-each (lambda (bucket) (p (##sys#slot bucket 0) (##sys#slot bucket 1)))
		      (##sys#slot ht i) ) ) ) )

(define (##sys#hash-table-size ht)
  (let loop ((len (##sys#size ht)) (bkt 0) (size 0))
    (if (fx= bkt len)
        size
        (loop len (fx+ bkt 1) (fx+ size (##sys#length (##sys#slot ht bkt)))))))

;;; Compile lambda to closure:

(define (eval-decorator p ll h cntr)
  (##sys#decorate-lambda
   p 
   (lambda (x) (and (not (##sys#immediate? x)) (##core#inline "C_lambdainfop" x)))
   (lambda (p i)
     (##sys#setslot 
      p i 
      (##sys#make-lambda-info 
       (let ((o (open-output-string)))
	 (write ll o)
	 (get-output-string o))))
     p) ) )

(define ##sys#unbound-in-eval #f)
(define ##sys#eval-debug-level (make-parameter 1))

(define compile-to-closure
  (let ([write write]
	[reverse reverse]
	[with-input-from-file with-input-from-file]
	[unbound (##sys#slot '##sys#arbitrary-unbound-symbol 0)]
	[display display] )
    (lambda (exp env se #!optional cntr evalenv static)

      (define (find-id id se)		; ignores macro bindings
	(cond ((null? se) #f)
	      ((and (eq? id (caar se)) (symbol? (cdar se))) (cdar se))
	      (else (find-id id (cdr se)))))

      (define (rename var se)
	(cond ((find-id var se))
	      ((##sys#get var '##core#macro-alias))
	      (else var)))

      (define (lookup var0 e se)
	(let ((var (rename var0 se)))
	  (d `(LOOKUP/EVAL: ,var0 ,var ,e ,(map (lambda (x) (car x)) se)))
	  (let loop ((envs e) (ei 0))
	    (cond ((null? envs) (values #f var))
		  ((posq var (##sys#slot envs 0)) => (lambda (p) (values ei p)))
		  (else (loop (##sys#slot envs 1) (fx+ ei 1))) ) ) ))

      (define (posq x lst)
	(let loop ((lst lst) (i 0))
	  (cond ((null? lst) #f)
		((eq? x (##sys#slot lst 0)) i)
		(else (loop (##sys#slot lst 1) (fx+ i 1))) ) ) )

      (define (emit-trace-info tf info cntr e v) 
	(when tf
	  (##core#inline 
	   "C_emit_eval_trace_info" 
	   info
	   (##sys#make-structure 'frameinfo cntr e v)
	   ##sys#current-thread) ) )
      
      (define (emit-syntax-trace-info tf info cntr) 
	(when tf
	  (##core#inline
	   "C_emit_syntax_trace_info"
	   info
	   cntr
	   ##sys#current-thread) ) )
	
      (define (decorate p ll h cntr)
	(eval-decorator p ll h cntr))

      (define (compile x e h tf cntr se)
	(cond ((keyword? x) (lambda v x))
	      ((symbol? x)
	       (receive (i j) (lookup x e se)
		 (cond ((not i)
			(let ((var (cond ((not (symbol? j)) x) ; syntax?
                                         ((not (assq x se))
                                          (and (not static)
                                               (##sys#alias-global-hook j #f cntr)))
                                         (else (or (##sys#get j '##core#primitive) j)))))
			  (when (and ##sys#unbound-in-eval
				     (or (not var)
					 (not (##sys#symbol-has-toplevel-binding? var))))
			    (set! ##sys#unbound-in-eval
			      (cons (cons var cntr) ##sys#unbound-in-eval)) )
			  (cond ((not var)
				 (lambda (v)
				   (##sys#error "unbound variable" x)))
				((##sys#symbol-has-toplevel-binding? var)
				 (lambda v (##sys#slot var 0)))
				(else
				 (lambda v (##core#inline "C_fast_retrieve" var))))))
                      (else
                       (case i
                         ((0) (lambda (v) 
                                (##sys#slot (##sys#slot v 0) j)))
                         ((1) (lambda (v) 
                                (##sys#slot (##sys#slot (##sys#slot v 1) 0) j)))
                         ((2) (lambda (v) 
                                (##sys#slot 
                                 (##sys#slot (##sys#slot (##sys#slot v 1) 1) 0)
                                 j)))
                         ((3) (lambda (v) 
                                (##sys#slot 
                                 (##sys#slot
                                  (##sys#slot (##sys#slot (##sys#slot v 1) 1) 1)
                                  0)
                                 j)))
                         (else
                          (lambda (v)
                            (##sys#slot (##core#inline "C_u_i_list_ref" v i) j))))))))
	      [(##sys#number? x)
	       (case x
		 [(-1) (lambda v -1)]
		 [(0) (lambda v 0)]
		 [(1) (lambda v 1)]
		 [(2) (lambda v 2)]
		 [else (lambda v x)] ) ]
	      [(boolean? x)
	       (if x
		   (lambda v #t)
		   (lambda v #f) ) ]
	      ((or (char? x)
		   (eof-object? x)
		   (string? x)
		   (blob? x)
		   (vector? x)
		   (##sys#srfi-4-vector? x))
	       (lambda v x) )
	      [(not (pair? x)) 
	       (##sys#syntax-error/context "illegal non-atomic object" x)]
	      [(symbol? (##sys#slot x 0))
	       (emit-syntax-trace-info tf x cntr)
	       (let ((x2 (expand x se)))
		 (d `(EVAL/EXPANDED: ,x2))
		 (if (not (eq? x2 x))
		     (compile x2 e h tf cntr se)
		     (let ((head (rename (##sys#slot x 0) se)))
		       ;; here we did't resolve ##core#primitive, but that is done in compile-call (via 
		       ;; a normal walking of the operator)
		       (case head

			 [(##core#quote)
			  (let* ((c (strip-syntax (cadr x))))
			    (case c
			      [(-1) (lambda v -1)]
			      [(0) (lambda v 0)]
			      [(1) (lambda v 1)]
			      [(2) (lambda v 2)]
			      [(#t) (lambda v #t)]
			      [(#f) (lambda v #f)]
			      [(()) (lambda v '())]
			      [else (lambda v c)] ) ) ]

			 ((##core#syntax)
			  (let ((c (cadr x)))
			    (lambda v c)))

			 [(##core#check)
			  (compile (cadr x) e h tf cntr se) ]

			 [(##core#immutable)
			  (compile (cadr x) e #f tf cntr se) ]
		   
			 [(##core#undefined) (lambda (v) (##core#undefined))]

			 [(##core#if)
			  (let* ([test (compile (cadr x) e #f tf cntr se)]
				 [cns (compile (caddr x) e #f tf cntr se)]
				 [alt (if (pair? (cdddr x))
					  (compile (cadddr x) e #f tf cntr se)
					  (compile '(##core#undefined) e #f tf cntr se) ) ] )
			    (lambda (v) (if (##core#app test v) (##core#app cns v) (##core#app alt v))) ) ]

			 [(##core#begin ##core#toplevel-begin)
			  (let* ((body (##sys#slot x 1))
				 (len (length body)) )
			    (case len
			      [(0) (compile '(##core#undefined) e #f tf cntr se)]
			      [(1) (compile (##sys#slot body 0) e #f tf cntr se)]
			      [(2) (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr se)]
					  [x2 (compile (cadr body) e #f tf cntr se)] )
				     (lambda (v) (##core#app x1 v) (##core#app x2 v)) ) ]
			      [else
			       (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr se)]
				      [x2 (compile (cadr body) e #f tf cntr se)] 
				      [x3 (compile `(##core#begin ,@(##sys#slot (##sys#slot body 1) 1)) e #f tf cntr se)] )
				 (lambda (v) (##core#app x1 v) (##core#app x2 v) (##core#app x3 v)) ) ] ) ) ]

			 [(##core#set!)
			  (let ((var (cadr x)))
			    (receive (i j) (lookup var e se)
			      (let ((val (compile (caddr x) e var tf cntr se)))
				(cond [(not i)
				       (when ##sys#notices-enabled
					 (and-let* ((a (assq var (##sys#current-environment)))
						    ((symbol? (cdr a))))
					   (##sys#notice "assignment to imported value binding" var)))
				       (let ((var
					      (if (not (assq x se)) ;XXX this looks wrong
						  (and (not static)
						       (##sys#alias-global-hook j #t cntr))
						  (or (##sys#get j '##core#primitive) j))))
					 (if (not var) ; static
					     (lambda (v)
					       (##sys#error 'eval "environment is not mutable" evalenv var)) ;XXX var?
					     (lambda (v)
					       (##sys#setslot var 0 (##core#app val v))) ) ) ]
				      [(zero? i) (lambda (v) (##sys#setslot (##sys#slot v 0) j (##core#app val v)))]
				      [else
				       (lambda (v)
					 (##sys#setslot
					  (##core#inline "C_u_i_list_ref" v i) j (##core#app val v)) ) ] ) ) ) ) ]

			 [(##core#let)
			  (let* ([bindings (cadr x)]
				 [n (length bindings)] 
				 [vars (map (lambda (x) (car x)) bindings)]
				 (aliases (map gensym vars))
				 [e2 (cons aliases e)]
				 (se2 (##sys#extend-se se vars aliases))
				 [body (compile-to-closure
					(##sys#canonicalize-body (cddr x) se2 #f)
					e2 se2 cntr evalenv static) ] )
			    (case n
			      [(1) (let ([val (compile (cadar bindings) e (car vars) tf cntr se)])
				     (lambda (v)
				       (##core#app body (cons (vector (##core#app val v)) v)) ) ) ]
			      [(2) (let ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					 [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app body (cons (vector (##core#app val1 v) (##core#app val2 v)) v)) ) ) ]
			      [(3) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					  [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] 
					  [t (cddr bindings)]
					  [val3 (compile (cadar t) e (caddr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app 
					body
					(cons (vector (##core#app val1 v) (##core#app val2 v) (##core#app val3 v)) v)) ) ) ]
			      [(4) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr se)]
					  [val2 (compile (cadadr bindings) e (cadr vars) tf cntr se)] 
					  [t (cddr bindings)]
					  [val3 (compile (cadar t) e (caddr vars) tf cntr se)] 
					  [val4 (compile (cadadr t) e (cadddr vars) tf cntr se)] )
				     (lambda (v)
				       (##core#app 
					body
					(cons (vector (##core#app val1 v)
						      (##core#app val2 v)
						      (##core#app val3 v)
						      (##core#app val4 v))
					      v)) ) ) ]
			      [else
			       (let ([vals (map (lambda (x) (compile (cadr x) e (car x) tf cntr se)) bindings)])
				 (lambda (v)
				   (let ([v2 (##sys#make-vector n)])
				     (do ([i 0 (fx+ i 1)]
					  [vlist vals (##sys#slot vlist 1)] )
					 ((fx>= i n))
				       (##sys#setslot v2 i (##core#app (##sys#slot vlist 0) v)) )
				     (##core#app body (cons v2 v)) ) ) ) ] ) ) ]

			 ((##core#letrec*)
			  (let ((bindings (cadr x))
				(body (cddr x)) )
			    (compile
			     `(##core#let
			       ,(##sys#map (lambda (b)
					     (list (car b) '(##core#undefined))) 
					   bindings)
			       ,@(##sys#map (lambda (b)
					      `(##core#set! ,(car b) ,(cadr b))) 
					    bindings)
			       (##core#let () ,@body) )
			     e h tf cntr se)))

			((##core#letrec)
			 (let* ((bindings (cadr x))
				(vars (map car bindings))
				(tmps (map gensym vars))
				(body (cddr x)) )
			   (compile
			    `(##core#let
			      ,(map (lambda (b)
				      (list (car b) '(##core#undefined))) 
				    bindings)
			      (##core#let ,(map (lambda (t b) (list t (cadr b))) tmps bindings)
					  ,@(map (lambda (v t)
						   `(##core#set! ,v ,t))
						 vars tmps)
					  (##core#let () ,@body) ) )
			      e h tf cntr se)))

			 [(##core#lambda)
			  (##sys#check-syntax 'lambda x '(_ lambda-list . #(_ 1)) #f se)
			  (let* ([llist (cadr x)]
				 [body (cddr x)] 
				 [info (cons (or h '?) llist)] )
			    (when (##sys#extended-lambda-list? llist)
			      (set!-values 
			       (llist body) 
			       (##sys#expand-extended-lambda-list 
				llist body ##sys#syntax-error-hook se) ) ) 
			    (##sys#decompose-lambda-list
			     llist
			     (lambda (vars argc rest)
			       (let* ((aliases (map gensym vars))
				      (se2 (##sys#extend-se se vars aliases))
				      (e2 (cons aliases e))
				      (body 
				       (compile-to-closure
					(##sys#canonicalize-body body se2 #f)
					e2 se2 (or h cntr) evalenv static) ) )
				 (case argc
				   [(0) (if rest
					    (lambda (v)
					      (decorate
					       (lambda r
						 (##core#app body (cons (vector r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda () (##core#app body (cons #f v)))
					       info h cntr) ) ) ]
				   [(1) (if rest
					    (lambda (v)
					      (decorate
					       (lambda (a1 . r)
						 (##core#app body (cons (vector a1 r) v)))
					       info h cntr) ) 
					    (lambda (v)
					      (decorate 
					       (lambda (a1)
						 (##core#app body (cons (vector a1) v)))
					       info h cntr) ) ) ]
				   [(2) (if rest
					    (lambda (v) 
					      (decorate
					       (lambda (a1 a2 . r)
						 (##core#app body (cons (vector a1 a2 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2)
						 (##core#app body (cons (vector a1 a2) v)))
					       info h cntr) ) ) ]
				   [(3) (if rest
					    (lambda (v) 
					      (decorate
					       (lambda (a1 a2 a3 . r)
						 (##core#app body (cons (vector a1 a2 a3 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3)
						 (##core#app body (cons (vector a1 a2 a3) v)))
					       info h cntr) ) ) ]
				   [(4) (if rest
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3 a4 . r)
						 (##core#app body (cons (vector a1 a2 a3 a4 r) v)))
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda (a1 a2 a3 a4)
						 (##core#app body (##sys#cons (##sys#vector a1 a2 a3 a4) v)))
					       info h cntr) ) ) ]
				   [else 
				    (if rest
					(lambda (v)
					  (decorate
					   (lambda as
					     (##core#app
					      body
					      (##sys#cons (apply ##sys#vector (fudge-argument-list argc as)) v)) )
					   info h cntr) )
					(lambda (v)
					  (decorate
					   (lambda as 
					     (let ([len (length as)])
					       (if (not (fx= len argc))
						   (##sys#error "bad argument count" argc len)
						   (##core#app body (##sys#cons (apply ##sys#vector as) v)))))
					   info h cntr) ) ) ] ) ) ) ) ) ]

			 ((##core#let-syntax)
			  (let ((se2 (append
				      (map (lambda (b)
					     (list
					      (car b)
					      se
					      (##sys#ensure-transformer
					       (##sys#eval/meta (cadr b))
					       (strip-syntax (car b)))))
					   (cadr x) ) 
				      se) ) )
			    (compile
			     (##sys#canonicalize-body (cddr x) se2 #f)
			     e #f tf cntr se2)))
			       
			 ((##core#letrec-syntax)
			  (let* ((ms (map (lambda (b)
					    (list
					     (car b)
					     #f
					     (##sys#ensure-transformer
					      (##sys#eval/meta (cadr b))
					      (strip-syntax (car b)))))
					  (cadr x) ) )
				 (se2 (append ms se)) )
			    (for-each 
			     (lambda (sb)
			       (set-car! (cdr sb) se2) )
			     ms) 
			    (compile
			     (##sys#canonicalize-body (cddr x) se2 #f)
			     e #f tf cntr se2)))
			       
			 ((##core#define-syntax)
			  (let* ((var (cadr x))
				 (body (caddr x))
				 (name (rename var se)))
			    (when (and static (not (assq var se)))
			      (##sys#error 'eval "environment is not mutable" evalenv var))
			    (##sys#register-syntax-export 
			     name (##sys#current-module)
			     body)	; not really necessary, it only shouldn't be #f
			    (##sys#extend-macro-environment
			     name
			     (##sys#current-environment)
			     (##sys#eval/meta body))
			    (compile '(##core#undefined) e #f tf cntr se) ) )

			 ((##core#define-compiler-syntax)
			  (compile '(##core#undefined) e #f tf cntr se))

			 ((##core#let-compiler-syntax)
			  (compile 
			   (##sys#canonicalize-body (cddr x) se #f)
			   e #f tf cntr se))

			 ((##core#include)
			  (compile
			   `(##core#begin
			     ,@(##sys#include-forms-from-file (cadr x)))
			   e #f tf cntr se))

			 ((##core#let-module-alias)
			  (##sys#with-module-aliases
			   (map (lambda (b)
				  (##sys#check-syntax 'functor b '(symbol symbol))
				  (strip-syntax b))
				(cadr x))
			   (lambda ()
			     (compile `(##core#begin ,@(cddr x)) e #f tf cntr se))))

			 ((##core#module)
			  (let* ((x (strip-syntax x))
				 (name (cadr x))
				 (exports 
				  (or (eq? #t (caddr x))
				      (map (lambda (exp)
					     (cond ((symbol? exp) exp)
						   ((and (pair? exp) 
							 (let loop ((exp exp))
							   (or (null? exp)
							       (and (symbol? (car exp))
								    (loop (cdr exp))))))
						    exp)
						   (else
						    (##sys#syntax-error-hook
						     'module
						     "invalid export syntax" exp name))))
					   (caddr x)))))
			    (when (##sys#current-module)
			      (##sys#syntax-error-hook 'module "modules may not be nested" name))
			    (parameterize ((##sys#current-module
					    (##sys#register-module name name exports))
					   (##sys#current-environment '())
					   (##sys#macro-environment 
					    ##sys#initial-macro-environment)
					   (##sys#module-alias-environment
					    (##sys#module-alias-environment)))
			      (##sys#with-property-restore
			       (lambda ()
				 (let loop ((body (cdddr x)) (xs '()))
				   (if (null? body)
				       (let ((xs (reverse xs)))
					 (##sys#finalize-module (##sys#current-module))
					 (##sys#provide name)
					 (lambda (v)
					   (let loop2 ((xs xs))
					     (if (null? xs)
						 (##sys#void)
						 (let ((n (cdr xs)))
						   (cond ((pair? n)
							  ((car xs) v)
							  (loop2 n))
							 (else
							  ((car xs) v))))))))
				       (loop 
					(cdr body)
					(cons (compile 
					       (car body) 
					       '() #f tf cntr 
					       (##sys#current-environment))
					      xs))))) ) )))

			 [(##core#loop-lambda)
			  (compile `(,(rename 'lambda se) ,@(cdr x)) e #f tf cntr se) ]

			 [(##core#require-for-syntax)
			  (if (##sys#symbol-has-toplevel-binding? 'chicken.load#load)
			      (let ((ids (map (lambda (x) (##sys#eval/meta x)) (cdr x))))
				(apply ##sys#require ids)
				(let ((rs (##sys#lookup-runtime-requirements ids)))
				  (compile
				   (if (null? rs)
				       '(##core#undefined)
				       `(##sys#require ,@(map (lambda (x) `(##core#quote ,x)) rs)))
				   e #f tf cntr se)))
			      (##sys#syntax-error-hook
			       'require-for-syntax
			       "cannot load extension for syntax -\
			        the `load' library must be imported" x))]

			 [(##core#require)
			  (compile
			   (let loop ((ids (map strip-syntax (cdr x))))
			     (if (null? ids)
				 '(##core#undefined)
				 (let-values (((exp _ _) (##sys#expand-require (car ids))))
				   `(##core#begin ,exp ,(loop (cdr ids))))))
			   e #f tf cntr se)]

			 [(##core#elaborationtimeonly ##core#elaborationtimetoo) ; <- Note this!
			  (##sys#eval/meta (cadr x))
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#compiletimetoo)
			  (compile (cadr x) e #f tf cntr se) ]

			 [(##core#compiletimeonly ##core#callunit) 
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#declare)
			  (##sys#notice "declarations are ignored in interpreted code" x)
			  (compile '(##core#undefined) e #f tf cntr se) ]

			 [(##core#define-inline ##core#define-constant)
			  (compile `(,(rename 'define se) ,@(cdr x)) e #f tf cntr se) ]
                   
			 [(##core#primitive ##core#inline ##core#inline_allocate ##core#foreign-lambda 
					    ##core#define-foreign-variable 
					    ##core#define-external-variable ##core#let-location
					    ##core#foreign-primitive ##core#location
					    ##core#foreign-lambda* ##core#define-foreign-type)
			  (##sys#syntax-error-hook "cannot evaluate compiler-special-form" x) ]

			 [(##core#app)
			  (compile-call (cdr x) e tf cntr se) ]

			 ((##core#the)
			  (compile (cadddr x) e h tf cntr se))
			 
			 ((##core#typecase)
			  ;; drops exp and requires "else" clause
			  (cond ((assq 'else (strip-syntax (cdddr x))) =>
				 (lambda (cl)
				   (compile (cadr cl) e h tf cntr se)))
				(else
				 (##sys#syntax-error-hook
				  'compiler-typecase
				  "no `else-clause' in unresolved `compiler-typecase' form"
				  x))))

			 (else
			  (fluid-let ((##sys#syntax-context (cons head ##sys#syntax-context)))
			    (compile-call x e tf cntr se)))))))]
	      
	      [else
	       (emit-syntax-trace-info tf x cntr)
	       (compile-call x e tf cntr se)] ) )

      (define (fudge-argument-list n alst)
	(if (null? alst) 
	    (list alst)
	    (do ((n n (fx- n 1))
		 (c 0 (fx+ c 1))
		 (args alst 
		       (if (eq? '() args)
			   (##sys#error "bad argument count" n c)
			   (##sys#slot args 1)))
		 (last #f args) )
		((fx= n 0)
		 (##sys#setslot last 1 (list args))
		 alst) ) ) )

      (define (checked-length lst)
	(let loop ([lst lst] [n 0])
	  (cond [(null? lst) n]
		[(pair? lst) (loop (##sys#slot lst 1) (fx+ n 1))]
		[else #f] ) ) )

      (define (compile-call x e tf cntr se)
	(let* ((head (##sys#slot x 0))
	       (fn (if (procedure? head) 
		       (lambda _ head)
		       (compile (##sys#slot x 0) e #f tf cntr se)))
	       (args (##sys#slot x 1))
	       (argc (checked-length args))
	       (info x) )
	  (case argc
	    [(#f) (##sys#syntax-error/context "malformed expression" x)]
	    [(0) (lambda (v)
		   (emit-trace-info tf info cntr e v)
		   ((##core#app fn v)))]
	    [(1) (let ([a1 (compile (##sys#slot args 0) e #f tf cntr se)])
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v))) ) ]
	    [(2) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v))) ) ]
	    [(3) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v))) ) ]
	    [(4) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr se)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr se)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr se)] 
			[a4 (compile (##core#inline "C_u_i_list_ref" args 3) e #f tf cntr se)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr e v)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v) (##core#app a4 v))) ) ]
	    [else (let ([as (##sys#map (lambda (a) (compile a e #f tf cntr se)) args)])
		    (lambda (v)
		      (emit-trace-info tf info cntr e v)
		      (apply (##core#app fn v) (##sys#map (lambda (a) (##core#app a v)) as))) ) ] ) ) )

      (compile exp env #f (fx> (##sys#eval-debug-level) 0) cntr se) ) ) )


;;; evaluate in the macro-expansion/compile-time environment
(define (##sys#eval/meta form)
  (let ((oldcm (##sys#current-module))
	(oldme (##sys#macro-environment))
	(oldce (##sys#current-environment))
	(mme (##sys#meta-macro-environment))
	(cme (##sys#current-meta-environment))
	(aee (##sys#active-eval-environment)))
    (dynamic-wind
	(lambda () 
	  (##sys#current-module #f)
	  (##sys#macro-environment mme)
	  (##sys#current-environment cme)
	  (##sys#active-eval-environment ##sys#current-meta-environment))
	(lambda ()
	  ((compile-to-closure
	    form
	    '() 
	    (##sys#current-meta-environment)) ;XXX evalenv? static?
	   '() ) )
	(lambda ()
	  (##sys#active-eval-environment aee)
	  (##sys#current-module oldcm)
	  (##sys#current-meta-environment (##sys#current-environment))
	  (##sys#current-environment oldce)
	  (##sys#meta-macro-environment (##sys#macro-environment))
	  (##sys#macro-environment oldme)))))

(define eval-handler
  (make-parameter
   (lambda (x #!optional env)
     (let ((se (##sys#current-environment)))
       (cond (env
	      (##sys#check-structure env 'environment 'eval)
	      (let ((se2 (##sys#slot env 2)))
		((if se2		; not interaction-environment?
		     (parameterize ((##sys#macro-environment '()))
		       (compile-to-closure x '() se2 #f env (##sys#slot env 3)))
		     (compile-to-closure x '() se #f env #f))
		 '() ) ) )
	     (else
	      ((compile-to-closure x '() se #f #f #f) '() ) ) ) ) )))

(define (eval x . env)
  (apply (eval-handler) x env))


;;; Environments:

(define interaction-environment
  (let ((e (##sys#make-structure 'environment 'interaction-environment #f #f)))
    (lambda () e)))

(define-record-printer (environment e p)
  (##sys#print "#<environment " #f p)
  (##sys#print (##sys#slot e 1) #f p)
  (##sys#write-char-0 #\> p))

(define scheme-report-environment)
(define null-environment)

(let* ((r4s (module-environment 'r4rs 'scheme-report-environment/4))
       (r5s (module-environment 'scheme 'scheme-report-environment/5))
       (r4n (module-environment 'r4rs-null 'null-environment/4))
       (r5n (module-environment 'r5rs-null 'null-environment/5)))
  (define (strip se)
    (foldr
     (lambda (s r)
       (if (memq (car s)
		 '(import
		   import-syntax
		   import-for-syntax
		   import-syntax-for-syntax
		   require-extension
		   require-extension-for-syntax
		   require-library
		   begin-for-syntax
		   export
		   module
		   cond-expand
		   syntax
		   reexport))
	   r
	   (cons s r)))
     '()
     se))
  ;; Strip non-std syntax from SEs
  (##sys#setslot r4s 2 (strip (##sys#slot r4s 2)))
  (##sys#setslot r4n 2 (strip (##sys#slot r4n 2)))
  (##sys#setslot r5s 2 (strip (##sys#slot r5s 2)))
  (##sys#setslot r5n 2 (strip (##sys#slot r5n 2)))
  (set! scheme-report-environment
    (lambda (n)
      (##sys#check-fixnum n 'scheme-report-environment)
      (case n
	((4) r4s)
	((5) r5s)
	(else
	 (##sys#error
	  'scheme-report-environment
	  "unsupported scheme report environment version" n)) ) ) )
  (set! null-environment
    (lambda (n)
      (##sys#check-fixnum n 'null-environment)
      (case n
	((4) r4n)
	((5) r5n)
	(else
	 (##sys#error
	  'null-environment
	  "unsupported null environment version" n))))))


;;; Setting properties dynamically scoped

(define-values (##sys#put/restore! ##sys#with-property-restore)
  (let ((trail '())
	(restoring #f))
    (values
     (lambda (sym prop val)
       (when restoring
	 (set! trail (cons (list sym prop (##sys#get sym prop)) trail)))
       (##sys#put! sym prop val)
       val)
     (lambda (thunk)
       (let ((t0 #f)
	     (r0 restoring))
	 (dynamic-wind
	     (lambda ()
	       (set! t0 trail)
	       (set! restoring #t))
	     thunk
	     (lambda ()
	       (do () ((eq? t0 trail))
		 (apply ##sys#put! (car trail))
		 (set! trail (cdr trail)))
	       (set! restoring r0))))))))


;;; Split lambda-list into its parts:

(define ##sys#decompose-lambda-list
  (let ([reverse reverse])
    (lambda (llist0 k)

      (define (err)
	(set! ##sys#syntax-error-culprit #f)
	(##sys#syntax-error-hook "illegal lambda-list syntax" llist0) )

      (let loop ([llist llist0] [vars '()] [argc 0])
	(cond [(eq? llist '()) (k (reverse vars) argc #f)]
	      [(not (##core#inline "C_blockp" llist)) (err)]
	      [(##core#inline "C_symbolp" llist) (k (reverse (cons llist vars)) argc llist)]
	      [(not (##core#inline "C_pairp" llist)) (err)]
	      [else (loop (##sys#slot llist 1)
			  (cons (##sys#slot llist 0) vars)
			  (fx+ argc 1) ) ] ) ) ) ) )

) ; eval module

;;; Simple invocation API:

(declare
  (hide store-result store-string
	CHICKEN_yield CHICKEN_eval CHICKEN_eval_string
	CHICKEN_eval_to_string CHICKEN_eval_string_to_string
	CHICKEN_apply CHICKEN_apply_to_string CHICKEN_eval_apply
	CHICKEN_read CHICKEN_get_error_message))


#>
#define C_store_result(x, ptr)   (*((C_word *)C_block_item(ptr, 0)) = (x), C_SCHEME_TRUE)
<#

(define (store-result x result)
  (##sys#gc #f)
  (when result
    (##core#inline "C_store_result" x result) )
  #t)

(define-external (CHICKEN_yield) bool
  (chicken.internal#run-safe (lambda () (begin (##sys#thread-yield!) #t))) )

(define-external (CHICKEN_eval (scheme-object exp) ((c-pointer "C_word") result)) bool
  (chicken.internal#run-safe
   (lambda ()
     (store-result (chicken.eval#eval exp) result))))

(define-external (CHICKEN_eval_string (c-string str) ((c-pointer "C_word") result)) bool
  (chicken.internal#run-safe
   (lambda ()
     (let ((i (open-input-string str)))
       (store-result (chicken.eval#eval (read i)) result)))))

#>
#define C_copy_result_string(str, buf, n)  (C_memcpy((char *)C_block_item(buf, 0), C_c_string(str), C_unfix(n)), ((char *)C_block_item(buf, 0))[ C_unfix(n) ] = '\0', C_SCHEME_TRUE)
<#

(define (store-string str bufsize buf)
  (let ((len (##sys#size str)))
    (cond ((fx>= len bufsize)
	   (set! chicken.internal#last-error
	     "Error: not enough room for result string")
	   #f)
	  (else
	   (##core#inline "C_copy_result_string" str buf len)))))

(define-external (CHICKEN_eval_to_string (scheme-object exp) ((c-pointer "char") buf)
					  (int bufsize))
  bool
  (chicken.internal#run-safe
   (lambda ()
     (let ((o (open-output-string)))
       (write (chicken.eval#eval exp) o)
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_eval_string_to_string (c-string str) ((c-pointer "char") buf)
						 (int bufsize) ) 
  bool
  (chicken.internal#run-safe
   (lambda ()
     (let ((o (open-output-string)))
       (write (chicken.eval#eval (read (open-input-string str))) o)
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_apply (scheme-object func) (scheme-object args) 
				 ((c-pointer "C_word") result))
  bool
  (chicken.internal#run-safe (lambda () (store-result (apply func args) result))) )

(define-external (CHICKEN_apply_to_string (scheme-object func) (scheme-object args) 
					   ((c-pointer "char") buf) (int bufsize))
  bool
  (chicken.internal#run-safe
   (lambda ()
     (let ((o (open-output-string)))
       (write (apply func args) o) 
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_read (c-string str) ((c-pointer "C_word") result)) bool
  (chicken.internal#run-safe
   (lambda ()
     (let ((i (open-input-string str)))
       (store-result (read i) result) ) ) ) )

(define-external (CHICKEN_get_error_message ((c-pointer "char") buf) (int bufsize)) void
  (store-string (or chicken.internal#last-error "No error") bufsize buf))
