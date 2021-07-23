;; Exercise 1.8
;;
;; Newton's method for cube roots is based on the fact that if y is an approximation to
;; the cube root of x, then a better approximation is given by the value
;; ((x/y^2 + 2y) / 3)
;;
;; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (square x)
  (* x x))

;; (square 3) ; 9
;; (square 5) ; 25

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

;; (improve 8 1000)  ; 253/24
;; (improve 9 1000)  ; 2458/243
;; (improve 10 1000) ; 10

;; The idea is to watch how guess changes from one iteration to the next and
;; to stop when the change is a very small fraction of the guess
(define (good-enough? prev-guess guess)
  (< (abs (- guess prev-guess)) 0.001))

;; (new-good-enough? 0 0.01)   ; false
;; (new-good-enough? 0 0.001)  ; false
;; (new-good-enough? 0 0.0001) ; true

;; We start with a value for the radicand and a value for the previous and current guesses.
;; If the current guess is good enough for the purpose, we are done;
;; if not, we must repeat the process with an improved guess.
(define (cube-root-iter prev-guess guess x)
  (if (good-enough? prev-guess guess)
      guess
      (cube-root-iter guess (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 100.0 1.0 x))

;; (cube-root 8)    ; 2.000000000012062
;; (cube-root 125)  ; 5.000000000287929
;; (cube-root 1000) ; 10.000000000000002
