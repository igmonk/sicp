;; Exercise 4.53
;;
;; With permanent-set! as described in exercise 4.51 and if-fail
;; as in exercise 4.52, what will be the result of evaluating

(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))


;; Below is the evaluation result:

;;; Starting a new problem 
;;; Amb-Eval value:
((8 35) (3 110) (3 20))

try-again ; There are no more values


;; Here, 'pairs' are treated as the accumulator for the pairs
;; of integers whose sum is prime.
;;
;; Each time a new pair that satisfies the requirements is found,
;; permanent-set! adds it to the accumulator 'pairs', but does not
;; rollback the state changes upon execution of the failure continuation,
;; that is triggered by (amb).
;;
;; As soon as all the choices of the first expression of if-fail
;; have been examined, its failure continuation goes into action,
;; which simply resolves to the value of the accumulated list.
