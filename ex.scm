;; S3C examples
;; ------------

(S3C) ;; henceforth code is in the S3C language

;; definition of a function that takes an int32 and returns an int32
(defn (factorial (int32 x)) int32
  (if (zero? x) 1
      ;; there's no support for generics, hence number operations need
      ;; to be selected manually: int32.int32.* multiplies two int32
      ;; numbers (returning int32, this is implicit)
      (int32.int32.* x (factorial (int32.dec x)))))

;; iterative version: not specifying tail-call optimization here,
;; though (and for factorial that won't matter that much):

;; Using unsigned integers here as it makes sense to exclude negative
;; numbers anyway.

(defn (factorial2-iter (uint32 z) (uint32 x)) uint32
  ;; zero? might be special in that even though it is generic, it can
  ;; be translated to C as "0 == x" and the C compiler will figure out
  ;; the width of the operation argument. The S3C compiler might also
  ;; be able to figure it out (the current macro-based implementation
  ;; can't), if so then generics which can be resolved statically
  ;; could be allowed.
  (if (zero? x)
      z
      ;; alternative naming convention for operations: 'namespace
      ;; prefix'. Makes sense for ops that take and return all of the
      ;; same types.
      (factorial2-iter (uint32:* z x) (uint32:dec x))))

(defn (factorial2 (uint32 x)) uint32
  (factorial2-iter 1 x))

;; the TEST form wraps tests in Scheme code, they are not compiled to
;; C, but run from the Scheme host system
(TEST
 ;;> (factorial -1)
 ;; out of stack mem
 ;;> (factorial2 -1)
 ;; out of stack mem -- or infinite loop if C compiler optimizes. Or rather, now,
 ;; *** ERROR IN factorial2, "ex.scm"@44.1 -- does not match uint32: -1
 > (define (t-factorial f)
     (local-TEST
      > (f 10)
      3628800
      > (f 0)
      1
      > (f 1)
      1
      > (f 2)
      2
      > (f 3)
      6))
 > (test (t-factorial s3c#factorial))
 > (test (t-factorial s3c#factorial2)))


(deftypealias nat0 uint64)
;; set this to uint32 ^ and see test failures (well, interruptions by
;; way of exceptions; should really use rules-based tests that catch
;; them instead) from the following code because of number overflows.


;; version that does not do tail-call optimization:

(defn (sumto-iter.1 (nat0 z) (nat0 x)) nat0
  ;; default-arithmetic sets the types to be used by the generic
  ;; arithmetic ops within its scope, to avoid having to write them
  ;; out like the int32.dec etc. above
  (default-arithmetic nat0
    (if (zero? x)
	z
	(sumto-iter.1 (+ z x) (dec x)))))


;; tail-call optimization using a trampoline (translated to a
;; trampoline in C): note that the return type can either be the
;; result, or a trampoline call (TC).

(defn (sumto-iter (nat0 z) (nat0 x)) (either nat0 TC?)
  (default-arithmetic nat0
    (if (zero? x)
	z
	;; fall back to the trampoline (i.e. return TC data structure)
	(TC sumto-iter (+ z x) (dec x)))))

(defn (sumto (nat0 x)) nat0
  ;; the trampoline, which bounces back as long as it receives a TC
  ;; data structure
  (trampoline (sumto-iter 0 x)))


;; tail-call optimization using a label (translated to a label,
;; variable mutation and goto in C):

(defn (sumto-iter2 (nat0 z) (nat0 x)) nat0
  (default-arithmetic nat0
   (label (lp z x)
	  (if (zero? x)
	      z
	      (lp (+ z x) (dec x))))))

(defn (sumto2 (nat0 x)) nat0
  (sumto-iter2 0 x))


(TEST
 ;;> (s3c#sumto-iter.1 0 100000)
 ;; out of stack mem
 > (s3c#sumto 100000)
 5000050000
 > (define (t-sumto f)
     (local-TEST
      > (f 0)
      0
      > (f 1)
      1
      > (f 2)
      3
      > (f 10)
      55))
 > (test (t-sumto s3c#sumto))
 > (test (t-sumto s3c#sumto2)))


;; data type definition:

(defstruct pair
  ;; without specification of a type, the generic object type (C's
  ;; void*) is assumed.
  car
  cdr)

;; constructor alias (hm, how will that work?)
(def cons pair)

;; an enumeration type with only one element (with the same name as
;; the type)
(defenum null
  null)

;; more restrictive list-building element than pair:
(defstruct listpair
  fst
  (list? rst))

(defenum listnull
  listnull)

(def list? (either (^ listpair?) listnull?))
;; XXX need a deftype form ? (which is just meddling with the phasing?)

;; Note: predicate naming convention can make as much sense, as type
;; names without the question mark. I'm not decided yet. Thus naming
;; conventions are inconsistent here.


;; [How to declare procedure types?

;; (defn (map fn l)
;;   (-> (-> (a) b) (list-of a) (list-of b))
;;   ;;(((a) -> b) -> (list-of a) -> (list-of b))
;;   )
;; Too deep type requirements?

;; (defn (map (procedure? fn) l)
;;   )
;; not enough for C. C needs to know enough for the type to be able to
;; call it.

;; ]

;; version that copies struct fields
(defn (map.1 ((procedure object object) fn)
	     (list? l)
	     (boolean? free-l))
  list?
  (if (listnull? l)
      listnull
      (let-listpair
       ;; copying field memory.  fst and rst are stack allocated. NOTE
       ;; that when copying mutable objects, mutations to the original
       ;; won't be visible in the copies and vice versa.
       ((fst rst) l)
       ;; now l can be freed
       (free-if l free-l)
       ;; |heap| allocates memory for the result of the given call on
       ;; the heap, instead of on the stack as would be done by
       ;; default.
       (listpair (fn fst) ;; <- copy fn's result, ok?
		 (heap (map.1 fn rst free-l))))))

;; version that accesses struct fields by pointer; this doesn't make
;; sense here where listpair has two word-sized fields anyway, but
;; it would make sense if listpair were parametrized by a type
;; wider than word (or if the algorithm were to mutate the
;; listpair contents).
(defn (map ((procedure object object) fn)
	   (list? l)
	   (boolean? free-l))
  list?
  (if (listnull? l)
      listnull
      (^let-listpair
       ((fst rst) l)
       ;; fst and rst are pointers to the struct elements. Can't free
       ;; l yet!

       ;; |returning| lets the constructor use the allocation received
       ;; by the caller, but allows to run statements afterwards:
       (returning
	;; If fn does not take a pointer, it needs to be dereferenced
	;; here:
	(listpair (fn (~ fst))
		  (heap (map fn (~ rst) free-l)))
	;; now we can free:
	(free-if l free-l)))))


;; the SCHEME form runs standard Scheme code, which is only available
;; during S3C's compile time or safe interpretation
(SCHEME
 (define (list->list l)
   (if (null? l)
       'listnull
       (let-pair ((a r) l)
		 (s3c#listpair a (list->list r))))))

(TEST
 > (s3c#map inc 'listnull #t)
 listnull
 > (s3c#map inc (list->list '(1 2)) #t)
 #(s3c#listpair 2 #(s3c#listpair 3 listnull))
 )

;; The above map variants only work when let-listpair* are
;; compiled into it; it needs a module system that allows to
;; parametrize the types, and then for every kind of list (different
;; size of the values) another instantiation is necessary. This might
;; blow up code size unduly (combinatorial explosion?), but perhaps
;; workable if flat struct nesting is used sparingly.

;; Alternative: use pointered code and take accessor functions as
;; arguments.

;; Aha: runtime instantiable modules would actually be enough for
;; pointered code!

;; Aha listpair is actually allocator (well not really, space
;; allocated by caller, but map is a caller of itself, hence needs to
;; know the size anyway[1]); stack allocation would be a problem
;; (*except* there is alloca!); but map needs heap allocation anyway.

;; [1] a huh yes need allocation size. That would need to be passed in
;; as argument, too, or again, instantiable modules would actually be
;; enough.

;; runtime instantiable modules means technically to pass every
;; function contained therein a context pointer holding the context
;; (parameter) values. (The functions are closures.) Same thing
;; 'performance-wise' of course.

