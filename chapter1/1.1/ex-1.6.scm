;; Exercise 1.6
;;
;; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
;; 'Why can't I just define it as an ordinary procedure in terms of cond?' she asks.
;; Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if.
;;
;; What happens when Alyssa attempts to use this to compute square roots? Explain.

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

;; A definition of 'if' as an ordinary procedure in terms of 'cond'
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; (new-if (= 2 3) 0 5) ; 5
;; (new-if (= 1 1) 0 5) ; 0

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; (sqrt 9) ; Aborting!: maximum recursion depth exceeded

;; Since the combination 'new-if' is no longer a primitive form as opposed to 'if',
;; its predicate, then-clause and else-clause are evaluated in accordance with
;; the applicative-order evaluation.
;; That effectively means 'sqrt-iter' will be invoking itself no matter what value
;; the predicate returns, which results in 'maximum recursion depth exceeded'.
