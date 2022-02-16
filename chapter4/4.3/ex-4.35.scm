;; Exercise 4.35
;;
;; Write a procedure an-integer-between that returns an integer
;; between two given bounds.
;;
;; This can be used to implement a procedure that finds
;; Pythagorean triples, i.e., triples of integers (i,j,k) between
;; the given bounds such that i <= j and i^2 + j^2 = k^2,
;; as follows:

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))


;; Tests
;;
;; Run in the driver loop. See: evaluator-test.scm
;;
;; (an-integer-between 1 5) ; 1
;;
;; try-again ; 2
;; try-again ; 3
;; try-again ; 4
;; try-again ; 5
;; try-again ; There are no more values
;;
;; (a-pythagorean-triple-between 1 20) ; (3 4 5)
;;
;; try-again ; (5 12 13)
;; try-again ; (6 8 10)
;; try-again ; (8 15 17)
;; try-again ; (9 12 15)
;; try-again ; (12 16 20)
;; try-again ; There are no more values
