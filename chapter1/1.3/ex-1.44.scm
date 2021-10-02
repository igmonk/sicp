;; Exercise 1.44
;;
;; The idea of smoothing a function is an important concept in signal processing.
;; If f is a function and dx is some small number, then the smoothed version of f
;; is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx).
;;
;; Write a procedure smooth that takes as input a procedure that computes f
;; and returns a procedure that computes the smoothed f.
;;
;; It is sometimes valuable to repeatedly smooth a function
;; (that is, smooth the smoothed function, and so on) to obtain the n-fold smoothed function.
;;
;; Show how to generate the n-fold smoothed function of any given function
;; using smooth and repeated from exercise 1.43.

(load "workbook.scm")
(load "ex-1.43.scm")

(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
	     (f x)
	     (f (+ x dx)))))

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth-n f n)
  ((repeated smooth n) f))

;; (inc 5)               ; 6
;; ((smooth inc) 5)      ; 6.
;; ((smooth-n inc 10) 5) ; 6.

;; (inc 0.00001)              ; 1.00001
;; ((smooth inc) 0.00001)     ; 1.0000099999999998
;; ((smooth-n inc 5) 0.00001) ; 1.0000099999999998

;; (square 2)              ; 4
;; ((smooth square) 2)     ; 4.000000000066667
;; ((smooth-n square 5) 2) ; 4.000000033333335

;; Moving average:
;; https://en.wikipedia.org/wiki/Moving_average
