;; Exercise 4.31
;;
;; The approach taken in this section is somewhat unpleasant,
;; because it makes an incompatible change to Scheme.
;;
;; It might be nicer to implement lazy evaluation as an upward-compatible
;; extension, that is, so that ordinary Scheme programs will work as before.
;;
;; We can do this by extending the syntax of procedure declarations
;; to let the user control whether or not arguments are to be delayed.
;;
;; While we're at it, we may as well also give the user the choice
;; between delaying with and without memoization.
;;
;; For example, the definition

(define (f a (b lazy) c (d lazy-memo))
  ...)

;; would define f to be a procedure of four arguments, where
;; the first and third arguments are evaluated when the procedure
;; is called, the second argument is delayed, and the fourth argument
;; is both delayed and memoized.
;;
;; Thus, ordinary procedure definitions will produce the same behavior
;; as ordinary Scheme, while adding the lazy-memo declaration to each
;; parameter of every compound procedure will produce the behavior of
;; the lazy evaluator defined in this section.
;;
;; Design and implement the changes required to produce such an extension
;; to Scheme. You will have to implement new syntax procedures to handle
;; the new syntax for 'define'.
;;
;; You must also arrange for eval or apply to determine when arguments
;; are to be delayed, and to force or delay arguments accordingly,
;; and you must arrange for forcing to memoize or not, as appropriate.


;; Start with differentiation between non-memoized and memoized thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))


;; Next, adjust 'force-it' to make it support different types of thunks
;; appropriately:

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk-memo? obj)
         (evaluate-thunk! obj actual-value))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;; Next, introduce the procedures answerable for discerning
;; the correct type of a parameter:

(define (param-pair? param)
  (and (pair? param) (= (length param) 2)))

(define (lazy-param? param)
  (and (param-pair? param) (eq? (cadr param) 'lazy)))

(define (lazy-memo-param? param)
  (and (param-pair? param) (eq? (cadr param) 'lazy-memo)))


;; The shape of a parameter defines the way its corresponding
;; argument is represented:
;;
;;             x -> the actual value
;;      (x lazy) -> a thunk
;; (x lazy-memo) -> a memoized thunk

(define (arg-exp->arg param arg-exp env)
  (cond ((symbol? param) (actual-value arg-exp env))
        ((lazy-param? param) (delay-it arg-exp env))
        ((lazy-memo-param? param) (delay-it-memo arg-exp env))
        (else
         (error "Unknown parameter type -- ARG-EXP->ARG" param))))


;; Implement a method that will transform the arguments to
;; a compound procedure based on its parameters:

(define (list-of-args params exps env)
  (if (or (null? params) (no-operands? exps))
      '()
      (cons (arg-exp->arg (car params) (first-operand exps) env)
            (list-of-args (cdr params) (rest-operands exps) env))))


;; Finally, adjust the compound-procedure? cond clause of apply
;; as follows:

<...>
((compound-procedure? procedure)
 (eval-sequence
  (procedure-body procedure)
  (extend-environment
   (param-names (procedure-parameters procedure))
   (list-of-args (procedure-parameters procedure)
                 arguments
                 env)
   (procedure-environment procedure))))
<...>

;; where param-names is a procedure that translates a parameter
;; to a symbolic parameter name:

(define (param-names params) (map param-name params))

(define (param-name param)
  (cond ((symbol? param) param)
        ((param-pair? param) (car param))
        (else
         (error "Unknown parameter type -- PARAM-NAME" param))))


;; See: thunk.scm
;;      evaluator.scm
;;      evaluator-test.scm
