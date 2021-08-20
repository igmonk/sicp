;; Exercise 1.38
;;
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis,
;; which included a continued fraction expansion for e - 2,
;; where e is the base of the natural logarithms.
;; In this fraction, the Ni are all 1,
;; and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... .
;;
;; Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e,
;; based on Euler's expansion.

;; Let's find out the dependency between the index i and Di
;;
;; 1  -> 1
;; 2  -> 2 = (i * 2 + 2) / 3
;; 3  -> 1
;; 4  -> 1
;; 5  -> 4 = (i * 2 + 2) / 3
;; 6  -> 1
;; 7  -> 1
;; 8  -> 6 = (i * 2 + 2) / 3
;; 9  -> 1
;; 10 -> 1
;; 11 -> 8 = (i * 2 + 2) / 3
;;
;; It can be noticed, for the indices whose modulo 3 gives 2 (i mod 3 = 2),
;; the following formula is used to get the value:
;;
;; (i * 2 + 2) / 3
;;
;; All the remaining indices are simply mapped to themselves in order get the corresponding value.

(define (e-minus-2 k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i)
	       (if (= (remainder i 3) 2)
		   (/ (+ (* i 2) 2) 3)
		   i))
	     k))

;; (e-minus-2 1)   ; 1.
;; (e-minus-2 2)   ; .6666666666666666
;; (e-minus-2 3)   ; .7
;; (e-minus-2 4)   ; .6976744186046512
;; (e-minus-2 5)   ; .6978021978021978
;; (e-minus-2 10)  ; .6977974628166747
;; (e-minus-2 100) ; .6977974628166803

;; (+ (euler 10) 2)   ; 2.6977974628166748
;; (+ (euler 100) 2)  ; 2.6977974628166805
;; (+ (euler 1000) 2) ; 2.6977974628166805


;; Utils: cont-frac

;; a. recursive process

(define (cont-frac n d k)
  (define (inner i)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (< i k)
	  (/ n-term (+ d-term (inner (+ i 1))))
	  (/ n-term d-term))))
  (inner 1))

;; b. iterative process

(define (cont-frac n d k)
  (define (iter i result)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (> i 0)
	  (iter (- i 1) (/ n-term (+ d-term result)))
	  result)))
  (iter k 0))
