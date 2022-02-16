;; Exercise 4.37
;;
;; Ben Bitdiddle claims that the following method for
;; generating Pythagorean triples is more efficient than
;; the one in exercise 4.35. Is he correct?
;;
;; (Hint: Consider the number of possibilities that must be explored.)

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))


;; Yes, he is correct.
;;
;; The overall number of possibilities that must be explored
;; has significantly reduced in comparison with that of ex. 4.35
;; due to:
;; - limitations imposed on the maximum value k-squared can take
;; - replacing the search of k (via amb in an-integer-between) by
;;   a trivial computation of the square root and its subsequent
;;   check for being integer


;; (a-pythagorean-triple-between 1 20) ; (3 4 5)
;;
;; try-again ; (5 12 13)
;; try-again ; (6 8 10)
;; try-again ; (8 15 17)
;; try-again ; (9 12 15)
;; try-again ; (12 16 20)
;; try-again ; There are no more values
