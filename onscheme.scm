;; quick and dirty S3C implementation


(defmacro (S3C . body)
  (namespace-expand 'S3C body))

(defmacro (s3c#SCHEME . body)
  (namespace-expand 'SCHEME body))



(defmacro (s3c#defn name+args rtype body0 . body)
  (mcase name+args
	 (`(`name . `args)
	  (expander#def
	   (cj-sourcify-deep
	    `(def (,name
		   ,@(map (lambda (arg)
			    (mcase arg
				   (list?
				    (list->vector (source-code arg)))
				   (else
				    arg)))
			  (source-code args)))
		  (-> ,rtype (begin ,body0 ,@body)))
	    stx)))))

(defmacro (s3c#defstruct name . fields)
  `(SCHEME
    (defstruct ,(source.symbol-append "s3c#" name)
      ,@(map (lambda (field)
	       (mcase field
		      (`(`type `name)
		       (vector (source.symbol-append "s3c#" type)
			       name))
		      (symbol?
		       field)))
	     fields))
    ;; need to alias let-foo since my defstruct doesn't know to handle
    ;; namespaces...:
    (defmacro (,(source.symbol-append "s3c#let-" name) . args)
      ;;(source.symbol-append 'expander#s3c#let- name) not available
      ;;yet when defined in same file
      (cons ',(source.symbol-append 'let-s3c# name) args))
    ;; and ^let- variant, identical for now
    (defmacro (,(source.symbol-append "s3c#^let-" name) . args)
      (cons ',(source.symbol-append 's3c#let- name) args))))

(defmacro (s3c#defenum name . members)
  `(SCHEME
    (defenum ,(source.symbol-append "s3c#" name)
      ,@members)
    ;; self-quoting constructors: (not part of my defenum, thus: -- XX
    ;; ah, actually it is, but to the wrong namespace)
    ,@(map (lambda (member)
	     (assert* symbol? member
		      (lambda_
		       `(define ,(symbol-append "s3c#" _)
			  ',_))))
	   members)))


;; need to maintain mapping, for s3c#default-arithmetic
(compile-time
 (def s3c-typealiases (make-table)))

(defmacro (s3c#deftypealias newname oldname)
  (assert* symbol? newname
	   (lambda (newname)
	     (assert* symbol? oldname
		      (lambda (oldname)
			(table-set! s3c-typealiases newname oldname)
			`(define ,newname ,oldname))))))


(compile-time
 (def 2ary-ops '(+ - * / quotient modulo))
 (def 1ary-ops '(inc dec)))

(defmacro (s3c#default-arithmetic t . body)
  (assert* symbol? t
	   (lambda (t)
	     (let ((t (table-ref s3c-typealiases t t)))
	       `(let ,(map (lambda (op)
			     `(,op ,(source.symbol-append t ":" op)))
			   (append 2ary-ops 1ary-ops))
		  ,@body)))))

(defmacro (s3c#TEST . args)
  `(SCHEME (TEST ,@args)))


(def s3c#either either)


(defstruct s3c#TC
  constructor-name: s3c#make-TC
  fn
  args)

;;(def *tc* #f)
(defmacro (s3c#TC fn . args)
  `(s3c#make-TC ,fn (##list ,@args)))

(define (s3c#trampoline v)
  (if (s3c#TC? v)
      (let-s3c#TC ((fn args) v)
	      (s3c#trampoline (apply fn args)))
      v))

(def s3c#zero? zero?) ;; does that need type?... Could rely on C compiler.

(insert-result-of
 (cons 'begin
       (map
	(lambda (t)
	  (let ((t? (source.symbol-append t '?)))
	    `(begin
	       ;; (define s3c#int32 int32?) etc.:
	       (define ,(source.symbol-append
			 "s3c#" t)
		 ,(source.symbol-append t '?))

	       ;; parametrized type safe 2ary op handling function:
	       (define (,(source.symbol-append
			  t '-op-2) op)
		 (typed-lambda (#(,t? a) #(,t? b))
			  (-> ,t? (op a b))))

	       ,@(map
		  (lambda (op)
		    (let ((cont
			   (lambda (op schemeop)
			     `(begin
				;; foo.foo.*
				(define ,(source.symbol-append
					  "s3c#" t "." t "." op)
				  (,(source.symbol-append
				     t '-op-2) ,schemeop))
				;; foo:*
				(define ,(source.symbol-append
					  "s3c#" t ":" op)
				  (,(source.symbol-append
				     t '-op-2) ,schemeop))
		  
				))))
		      ;; special case
		      (if (eq? op '/)
			  (cont op 'quotient)
			  (cont op op))))
		  2ary-ops)

	       ;; parametrized type safe 1ary op handling function:
	       (define (,(source.symbol-append
			  t '-op-1) op)
		 (typed-lambda (#(,t? a))
			       (-> ,t? (op a))))

	       ,@(map
		  (lambda (op)
		    (let ((cont
			   (lambda (op schemeop)
			     `(begin
				;; foo.*
				(define ,(source.symbol-append
					  "s3c#" t "." op)
				  (,(source.symbol-append
				     t '-op-1) ,schemeop))
				;; foo:*
				(define ,(source.symbol-append
					  "s3c#" t ":" op)
				  (,(source.symbol-append
				     t '-op-1) ,schemeop))))))
		      (if (pair? op)
			  (cont (car op) (cdr op))
			  (cont op op))))
		  1ary-ops))))
	'(int32 uint32 int8 uint8 int16 uint16 int64 uint64))))

(defmacro (s3c#label name+vars . body)
  (mcase name+vars
	 (`(`name . `vars)
	  (let ((fnname (source-symbol-append '__LABEL_ name)))
	    `(##let ,fnname ()
		    (##define-syntax
		     ,name
		     (lambda (stx)
		       (cj-sourcify-deep
			`(begin
			   ,@(map (lambda (v e)
				    `(set! ,v ,e))
				  ',vars
				  (cdr (source-code stx)))
			   (,',fnname))
			stx)))
		    ,@body)))))

(defmacro (s3c#returning e1 . es)
  (with-gensym res
	       `(let ((,res ,e1))
		  ,@es
		  ,res)))

(defmacro (s3c#heap expr)
  ;; XX for now
  expr)

;; take reference (pointer)
(defmacro (s3c#^ expr)
  ;; XX for now
  expr)

;; dereference. precious symbol?
(defmacro (s3c#~ expr)
  ;; XX for now
  expr)


(def tuple-of list-of) ;; for argument checking, that is, ok ?

;; use |->| for this? would be nice. But then, distinguish procedure
;; vs. function?  (Or rather use monads for that?)
;; (def (function . types)
;;      ;; (named rec
;;      ;; 	    (lambda vs
;;      ;; 	      ))
;;      (fold-right (lambda (type rest)
;; 		   (lambda vs
;; 		     (if (type vs)
;; 			 )
;; 		     		     ))))

;; (def (function argtypes resulttype)
;;      ;; heh another issue for TCO, but that's fine now that we don't
;;      ;; promise TCO anyway.
;;      )

(def (s3c#procedure . args)
     ;; XXX do more? Check at least as much as C will have to rely on!
     procedure?)

(def s3c#object true/1)

(def (s3c#free obj)
     ;; XX mark it etc.
     (void))

(def (s3c#free-if obj #(boolean? free?))
     (if free?
	 (s3c#free obj)))


;;XX what about naming. For porting Scheme code, "?" syntax would
;;actually be nice.
(def s3c#boolean? boolean?)

(def s3c#vector-ref vector-ref)
(def s3c#error error)

