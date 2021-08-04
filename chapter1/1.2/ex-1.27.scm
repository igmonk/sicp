;; Exercise 1.27
;;
;; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test.
;; That is, write a procedure that takes an integer n and tests whether a^n is congruent to
;; a modulo n for every a<n, and try your procedure on the given Carmichael numbers.
;;
;; Footnote 27
;; Numbers that fool the Fermat test are called Carmichael numbers,
;; and little is known about them other than that they are extremely rare.
;; There are 255 Carmichael numbers below 100,000,000.
;; The smallest few are 561, 1105, 1729, 2465, 2821, and 6601.
;; In testing primality of very large numbers chosen at random, the chance of stumbling upon
;; a value that fools the Fermat test is less than the chance that cosmic radiation will cause
;; the computer to make an error in carrying out a 'correct' algorithm.
;; Considering an algorithm to be inadequate for the first reason but not for the second
;; illustrates the difference between mathematics and engineering.

(define (fermat-test-full n)
  (fermat-test-full-iter n 1))

(define (fermat-test-full-iter n a)
  (cond ((= n a) true)
	((fermat-test n a) (fermat-test-full-iter n (+ a 1)))
	(else false)))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))

;; (fermat-test-full 561)  ; true
;; (fermat-test-full 1105) ; true
;; (fermat-test-full 1729) ; true
;; (fermat-test-full 2465) ; true
;; (fermat-test-full 2821) ; true
;; (fermat-test-full 6601) ; true
