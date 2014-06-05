;; quick and dirty S3C implementation

;; stuff needed by onscheme.scm at compile time? somehow necessary to
;; put into separate unit. XX Don't know why |both-times| won't work
;; for this.


(defenum s3namespace SCHEME S3C)

(def (namespace-form-for sym)
     (xcase sym
	    ((SCHEME) `(##namespace ("")))
	    ((S3C) `(begin
		      (##namespace ("s3c#"))
		      (##namespace ("" lambda typed-lambda def define-typed
				    define begin -> type-check
				    let if set!
				    ;;quote ;; hmm, only allow it for symbols?
				    ))))))

(def *cur-namespace* 'SCHEME)

;; NOTE: (SCHEME ...) is a statement, not an expression, no value is
;; returned.
(def (namespace-expand to-nssym body)
     (if (null? body)
	 (begin
	   (set! *cur-namespace* to-nssym)
	   (namespace-form-for to-nssym))
	 `(begin
	    ,(namespace-form-for to-nssym)
	    ,@body
	    ,(namespace-form-for *cur-namespace*))))

(TEST
 > (namespace-expand 'S3C '())
 (begin (##namespace ("s3c#"))
	(##namespace
	 (""
	  lambda
	  typed-lambda
	  def
	  define-typed
	  define
	  begin
	  ->
	  type-check
	  let
	  if
	  set!)))
 ;; XXX should switch back to default upon next compilation
 ;; unit. extend compiler...
 > (namespace-expand 'SCHEME '())
 (##namespace (""))
 > (namespace-expand 'S3C '(a b c))
 (begin (begin (##namespace ("s3c#"))
	       (##namespace
		(""
		 lambda
		 typed-lambda
		 def
		 define-typed
		 define
		 begin
		 ->
		 type-check
		 let
		 if
		 set!)))
	a b c (##namespace (""))))


