(+ 1 2 3) ; 6

(define pi 3.14159)
(define radius 10)

;; (* pi (* radius radius)) ; 314.159

(define circumference (* 2 pi radius))

;; circumference ; 62.8318

(define (square x) (* x x))

;; (square 5)                ; 25
;; (square (square 5))       ; 625
;; (+ (square 3) (square 4)) ; 25

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; (sum-of-squares 3 4)   ; 25
;; (sum-of-squares 30 40) ; 2500

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

;; (abs 100)  ; 100
;; (abs 0)    ; 0
;; (abs -100) ; 100

(define (>= x y)
  (or (> x y) (= x y)))

;; (>= 10 5) ; true
;; (>= 5 10) ; false

(define (average x y)
  (/ (+ x y) 2))

;; (average 1 3)  ; 2
;; (average -1 1) ; 0

;; A guess is improved by averaging it with the quotient of the radicand and the old guess.
(define (improve guess x)
  (average guess (/ x guess)))

;; (improve 1 2) ; 3/2

;; The idea is to improve the answer until it is close enough so that its square
;; differs from the radicand by less than a predetermined tolerance (here 0.001).
(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.001))

;; (good-enough? 10 100)      ; true
;; (good-enough? 9.9 100)     ; false
;; (good-enough? 9.99999 100) ; true

;; We start with a value for the radicand and a value for the guess.
;; If the guess is good enough for the purpose, we are done;
;; if not, we must repeat the process with an improved guess.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; (sqrt 9)                         ; 3.00009155413138
;; (sqrt (+ (square 3) (square 4))) ; 5.000023178253949

;; Internal definitions and block structure
;;
;; Block structure (nesting of definitions) and lexical scoping in action:
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (square guess))) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; (sqrt 9)   ; 3.00009155413138
;; (sqrt 100) ; 10.000000000139897
