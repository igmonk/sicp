;; Exercise 1.35
;;
;; Show that the golden ratio φ (section 1.2.2) is a fixed point of
;; the transformation x -> 1 + 1/x, and use this fact to compute φ
;; by means of the fixed-point procedure.


;; The golden ratio satisfies the equation
;;
;; φ^2 = φ + 1
;;
;; which can be transformed to
;;
;; φ = 1 + 1/φ
;;
;; by dividing both sides by φ
;;
;; Thus, the value of the golden ratio could be found using
;; the transformation x -> 1 + 1/x and the fixed-point procedure as follows:

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;; (golden-ratio) ; 1.6180327868852458


;; Utils

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
