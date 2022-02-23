;; Exercise 4.51
;;
;; Implement a new kind of assignment called permanent-set!
;; that is not undone upon failure.
;;
;; For example, we can choose two distinct elements from a list
;; and count the number of trials required to make a successful
;; choice as follows:

(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
;;; Starting a new problem
;;; Amb-Eval value:
(a b 2)
;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(a c 3)

;; What values would have been displayed if we had used set! here
;; rather than permanent-set! ?


;; The key difference between the permanent assignment operation
;; and its ordinal counterpart is that the former does not restore
;; the old value of the variable before continuing the failure:

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (_analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))


;; Tests (run in the driver loop)

(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 2)

try-again ; (a c 3)
try-again ; (b a 4)
try-again ; (b c 6)
try-again ; (c a 7)
try-again ; (c b 8)
try-again ; There are no more values


;; Same, but with set! instead of permanent-set!
(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 1)

try-again ; (a c 1)
try-again ; (b a 1)
try-again ; (b c 1)
try-again ; (c a 1)
try-again ; (c b 1)
try-again ; There are no more values

;; As the output confirms, each time the amb evaluator is asked
;; to produce a new value, the state of the counter gets restored
;; by the failure continuation of the ordinary form set!.
