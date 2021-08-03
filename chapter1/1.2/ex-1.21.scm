;; Exercise 1.21
;;
;; Use the smallest-divisor procedure to find the smallest divisor
;; of each of the following numbers: 199, 1999, 19999.

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((< n (square divisor)) n)
	((divisible? n divisor) divisor)
	(else (find-divisor n (+ divisor 1)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

(define (square x)
  (* x x))

;; (prime? 101) ; true
;; (prime? 102) ; false

;; (smallest-divisor 199)   ; 199
;; (smallest-divisor 1999)  ; 1999
;; (smallest-divisor 19999) ; 7
