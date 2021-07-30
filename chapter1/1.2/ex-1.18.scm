;; Exercise 1.18
;;
;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates
;; an iterative process for multiplying two integers in terms of adding, doubling,
;; and halving and uses a logarithmic number of steps.

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast*-iter a b product)
  (cond ((= b 0) product)
	((even? b) (fast*-iter (double a) (halve b) product)) ;; a * b = 2a * b/2
	(else (fast*-iter a (- b 1) (+ a product)))))

(define (fast* a b)
  (fast*-iter a b 0))

;; (fast* 0 0)   ; 0
;; (fast* 0 1)   ; 0
;; (fast* 1 1)   ; 1
;; (fast* 1 10)  ; 10
;; (fast* 10 10) ; 100
;; (fast* 5 25)  ; 125
