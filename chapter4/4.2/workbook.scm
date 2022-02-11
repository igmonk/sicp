;; 4.2 Variations on a Scheme - Lazy Evaluation

;; 4.2.1 Normal Order and Applicative Order
;;
;; Evaluation orders:
;;
;; - applicative order - all the arguments to procedures are evaluated
;;                       when the procedure is applied
;; - normal order - evaluation of procedure arguments is delayed until
;;                  the actual argument values are needed
;;
;; Scheme is an applicative-order language.

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; (let ((a 0) (b 0))
;;   (unless (= b 0)
;;     (/ a b)
;;     (begin (display "exception: returning 0")
;;            0)))
;;
;; Division by zero signalled by /

;; This won't work in an applicative-order language because both
;; the usual value and the exceptional value will be evaluated
;; before unless is called.
;;
;; An advantage of lazy evaluation is that some procedures,
;; such as unless, can do useful computation even if
;; evaluation of some of their arguments would produce errors
;; or would not terminate.


;; 4.2.2 An Interpreter with Lazy Evaluation
;;
;; With lazy evaluation, when applying a procedure,
;; the interpreter must determine which arguments
;; are to be evaluated and which are to be delayed.
;;
;; A 'thunk' is an object that contains the information
;; required to produce the value of the argument when
;; it is needed.
;;
;; The process of evaluating the expression in a thunk
;; is called 'forcing'.

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))


;; Representing thunks
;;
;; A thunk must package an expression together with the environment,
;; so that the argument can be produced later.
;;
;; To force the thunk, the expression and environment are extracted
;; from the thunk and the expression is evaluated in the environment.

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))


;; Memoized thunks
;;
;; A thunk can be 'memoized' - the first time a thunk is forced,
;; it stores the value that is computed;
;; subsequent forcings simply return the stored value without
;; repeating the computation.

;; When a thunk is forced, we will turn it into an evaluated thunk
;; by replacing the stored expression with its value and changing
;; the thunk tag so that it can be recognized as already evaluated.

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '())    ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; Notice that we also erase the env from the thunk
;; once the expression's value has been computed.
;;
;; This makes no difference in the values returned by the interpreter.
;; It does help save space, however, because removing the reference
;; from the thunk to the env once it is no longer needed
;; allows this structure to be garbage-collected and its space recycled,
;; as discussed in section 5.3.


;; Streams as Lazy Lists
;;
;; With lazy evaluation, streams and lists can be identical, so
;; there is no need for special forms or for separate list and
;; stream operations.
;;
;; All we need to do is to arrange matters so that cons is non-strict.
;; One way to accomplish this is to extend the lazy evaluator to allow for
;; non-strict primitives, and to implement cons as one of these.
;;
;; An easier way is to recall (section 2.1.3) that there is no
;; fundamental need to implement cons as a primitive at all.
;;
;; Instead, we can represent pairs as procedures:

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

;; Or, in case of an upward-compatible extension (ex. 4.31):

(define (cons (x lazy-memo) (y lazy-memo))
  (lambda (m) (m x y)))

(define (car (z lazy-memo)) (z (lambda (p q) p)))
(define (cdr (z lazy-memo)) (z (lambda (p q) q)))

;; In terms of these basic operations, the standard definitions of
;; the list operations will work with infinite lists (streams)
;; as well as finite ones, and the stream operations can be
;; implemented as list operations.


;; Note that these lazy lists are even lazier than the streams
;; of chapter 3: the 'car' of the list, as well as the 'cdr',
;; is delayed. This permits us to create delayed versions of
;; more general kinds of list structures, not just sequences.
;;
;; Hughes 1990 discusses some applications of "lazy trees".

;; In fact, even accessing the 'car' or 'cdr' of a lazy pair
;; need not force the value of a list element.
;;
;; The value will be forced only when it is really needed - e.g.,
;; for use as the argument of a primitive, or to be printed as an answer.
