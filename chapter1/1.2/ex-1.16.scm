;; Exercise 1.16
;;
;; Design a procedure that evolves an iterative exponentiation process that uses
;; successive squaring and uses a logarithmic number of steps, as does fast-expt.
(define (expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n product)
  (cond ((= n 0) product)
	((even? n) (fast-expt-iter (square b) (/ n 2) product)) ;; b^n = (b^2)^n/2
	(else (fast-expt-iter b (- n 1) (* product b)))))

(define (even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

;; (expt 2 3)  ; 8
;; (expt 2 10) ; 1024
