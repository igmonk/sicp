;; Exercise 1.28
;;
;; One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test
;; (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little Theorem,
;; which states that if n is a prime number and a is any positive integer less than n,
;; then a raised to the (n - 1)st power is congruent to 1 modulo n.
;; To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and
;; raise a to the (n - 1)st power modulo n using the expmod procedure.
;; However, whenever we perform the squaring step in expmod, we check to see if
;; we have discovered a 'nontrivial square root of 1 modulo n', that is, a number
;; not equal to 1 or n - 1 whose square is equal to 1 modulo n.
;; It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime.
;; It is also possible to prove that if n is an odd number that is not prime, then,
;; for at least half the numbers a<n, computing an-1 in this way will reveal a nontrivial square root
;; of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.)
;;
;; Modify the expmod procedure to signal if it discovers a nontrivial square root of 1,
;; and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test.
;; Check your procedure by testing various known primes and non-primes.
;; Hint: One convenient way to make expmod signal is to have it return 0.

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((mr-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (mr-test n)
  (define (try-it a)
    (= (mr-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-expmod base exp m)
  (define (mr-check a)
    (square-root-check a (remainder (square a) m)))
  (define (square-root-check a square-mod)
    (if (and (not (= a 1))
	     (not (= a (- m 1)))
	     (= square-mod 1))
	0
	square-mod))
  (cond ((= exp 0) 1)
        ((even? exp)
	 (mr-check (mr-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (mr-expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))


;; Small numbers
;;
;; (fast-prime? 2 2)  ; true
;; (fast-prime? 3 3)  ; true
;; (fast-prime? 4 4)  ; false
;; (fast-prime? 5 5)  ; true
;; (fast-prime? 6 5)  ; false
;; (fast-prime? 7 5)  ; true
;; (fast-prime? 8 5)  ; false
;; (fast-prime? 9 5)  ; false
;; (fast-prime? 10 5) ; false
;; (fast-prime? 11 5) ; true

;; Prime numbers (ex. 1.22)
;;
;; (fast-prime? 10000000061 1000)     ; true
;; (fast-prime? 100000000057 1000)    ; true
;; (fast-prime? 1000000000063 1000)   ; true
;; (fast-prime? 10000000000099 1000)  ; true
;; (fast-prime? 100000000000097 1000) ; true

;; Carmichael numbers:
;;
;; (fast-prime? 561 100)  ; false
;; (fast-prime? 1105 100) ; false
;; (fast-prime? 1729 100) ; false
;; (fast-prime? 2465 100) ; false
;; (fast-prime? 2821 100) ; false
;; (fast-prime? 6601 100) ; false
