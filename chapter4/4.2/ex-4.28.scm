;; Exercise 4.28
;;
;; Eval uses 'actual-value' rather than 'eval' to evaluate
;; the operator before passing it to 'apply', in order to
;; force the value of the operator.
;;
;; Give an example that demonstrates the need for this forcing.


;; The reason this forcing is needed is that an evaluated
;; expression might itself be a thunk, which, if not forced,
;; might lead to an exception being thrown.
;;
;; This can be demonstrated by making evaluator produce a thunk
;; where the actual value of an operator - a procedure - is needed.

;; Consider the following example:

((lambda (adder)
   (adder 1 2))
 (lambda (x y)
   (+ x y))) ; 3

;; Given the normal-order evaluator that uses 'actual-value',
;; the expression evaluates to 3 (same as in MIT Scheme).
;;
;; A procedure that takes two parameters - x and y - is passed
;; as argument to another procedure that invokes the former
;; with 1 and 2, computing the sum of these numbers.


;; With the normal-order evaluator that uses 'eval' instead of
;; 'actual-value' (that forces what 'eval' returns),
;; the lambda that adds two numbers gets transformed into a thunk
;; (being an argument to another compound procedure).
;;
;; Upon evaluation, this thunk is what the parameter 'adder'
;; resolves to, but never gets forced.
;; That leads to the application of the thunk rather than
;; the application of the procedure it evaluates to.
;;
;; Hence, the following error is thrown:

;Unknown procedure type -- APPLY (thunk (lambda ... ...) ...)
