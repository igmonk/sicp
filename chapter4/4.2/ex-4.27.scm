;; Exercise 4.27
;;
;; Suppose we type in the following definitions to the lazy evaluator:

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

;; Give the missing values in the following sequence of interactions,
;; and explain your answers:

(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
<response>
;;; L-Eval input:
w
;;; L-Eval value:
<response>
;;; L-Eval input:
count
;;; L-Eval value:
<response>


;; The result depends on how a sequence of expressions
;; is evaluated, either:
;; - each of its expressions is forced
;; - each but he final one is forced


;; 1. Each expression is forced
;;    (Optionally: force the value returned by lookup
;;                 in the (variable? exp) eval's clause)

;;; L-Eval input:
count
;;; L-Eval value:
2
;;; L-Eval input:
w
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
2

;; As can be seen, the results are the same as in the case
;; of the applicative-order evaluator.


;; 2. All expressions in the sequence are forced except
;;    the final one

;;; L-Eval input:
count
;;; L-Eval value:
1
;;; L-Eval input:
w
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
2

;; The output above is explained by the fact that
;; during evaluation of
;;
;; (define w (id (id 10)))
;;
;; the argument to id - (id 10) - becomes a thunk that
;; does not get forced, and, therefore, the body of id
;; gets evaluated only once, making count equal to 1.
;;
;; Later, when the variable w gets accessed,
;; driver-loop forces the thunk that is associated with it,
;; incrementing count a second time.
