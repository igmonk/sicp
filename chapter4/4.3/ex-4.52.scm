;; Exercise 4.52
;;
;; Implement a new construct called 'if-fail' that permits the user
;; to catch the failure of an expression.
;;
;; If-fail takes two expressions.
;; It evaluates the first expression as usual and returns as usual
;; if the evaluation succeeds.
;; If the evaluation fails, however, the value of the second expression
;; is returned, as in the following example:

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
all-odd
;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
8


;; The solution could be a simplified version of 'if',
;; where there is no predicate, but only two expressions,
;; of which the second gets evaluated as part of the failure
;; continuation of the first:

(define (analyze-if-fail exp)
  (let ((exp1-proc (_analyze (if-fail-exp1 exp)))
        (exp2-proc (_analyze (if-fail-exp2 exp))))
    (lambda (env succeed fail)
      (exp1-proc env
                 (lambda (exp1-value fail2)
                   (succeed exp1-value fail2))
                 (lambda ()
                   (exp2-proc env succeed fail))))))


;; Below are the corresponding selectors and constructor:

(define (make-if-fail exp1 exp2)
  (list 'if-fail exp1 exp2))

(define (if-fail-exp1 exp) (cadr exp))
(define (if-fail-exp2 exp) (caddr exp))


;; Tests

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;;; Amb-Eval value:
all-odd

try-again ; There are no more value


;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
8

try-again ; all-odd
try-again ; There are no more values
