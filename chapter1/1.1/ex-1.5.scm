;; Exercise 1.5
;;
;; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with
;; is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Then he evaluates the expression
(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
;;
;; The evaluation process will end up with endless recursion since (y) is evaluated to the procedure that unconditionally invokes itself before the operation (test) is applied.

;; What behavior will he observe with an interpreter that uses normal-order evaluation?
;;
;; The evaluation process will result in 0 since the operand (y) that points to (p) isn't evaluated until the (test) operand is fully expanded.
;; The special form (if) will evaluate the predicate expression, and, subsequently, the consequent, leaving the alternative expression untouched.
