;; Exercise 1.46
;;
;; Several of the numerical methods described in this chapter are instances of
;; an extremely general computational strategy known as 'iterative improvement'.
;;
;; Iterative improvement says that, to compute something, we start with an initial guess
;; for the answer, test if the guess is good enough, and otherwise improve the guess
;; and continue the process using the improved guess as the new guess.
;;
;; Write a procedure 'iterative-improve' that takes two procedures as arguments:
;; - a method for telling whether a guess is good enough and
;; - a method for improving a guess
;;
;; iterative-improve should return as its value a procedure that takes a guess as argument
;; and keeps improving the guess until it is good enough.
;;
;; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3
;; in terms of iterative-improve.

(load "workbook.scm")

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (let ((next-guess (improve-guess guess)))
      (if (good-enough? guess next-guess)
	  next-guess
	  (iter (improve-guess next-guess)))))
  (lambda (guess) (iter guess)))


;; sqrt in terms of iterative-improve

(define (sqrt n)
  (define (improve-guess guess)
    (average guess (/ n guess)))
  ((iterative-improve close-enough? improve-guess) 1.0))

;; (sqrt 4)   ; 2.000000000000002
;; (sqrt 16)  ; 4.
;; (sqrt 256) ; 16.


;; fixed-point in terms of iterative-improve

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

;; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) ; 1.2587479705055356


;; Utils

(define (close-enough? prev-guess guess)
  (< (abs (- guess prev-guess)) 0.0001))
