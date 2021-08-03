;; Exercise 1.25
;;
;; Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod.
;; After all, she says, since we already know how to compute exponentials,
;; we could have simply written:

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; where fast-expt is defined as:

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))

;; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.


;; By making use of fast-expt, before taking the number modulo,
;; expmod fully calculates exponents first which greatly affects (slows down)
;; the speed of calculations involving large numbers, which is shown below:

;; (expmod 5 10 5)        ; 0
;; (expmod 5 1000000 5)   ; 0
;; (expmod 25 1000000 25) ; 0 <- Takes several seconds to compute

;; This is known as the Direct method of Modular exponentiation.
;;
;; Although, the result is correct, given not an insignificant input (for ex.: 25^1000000),
;; it takes several secoonds to execute the function.
;; The speed of execution is not acceptable with even larger numbers.

;; Running the same test against the original implementation of expmod
;; returns the result much faster:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;; (expmod 25 1000000 25) ; 0 <- Takes a few milliseconds to compute

;; This is known as the Right-to-left method of Modular exponentiation -
;; a combination of the Memory-efficient method and exponentiation by squaring.
;; This method drastically reduces the number of operations to perform modular exponentiation,
;; while keeping the same memory footprint as in the Memory-efficient method.
;;
;; In its turn, the Memory-efficient method makes use of the identity
;; (a*b) mod m = [(a mod m) * (b mod m)] mod m
;;
;; Keeping the numbers smaller requires additional modular reduction operations,
;; but the reduced size makes each operation faster, saving time (as well as memory) overall.
;;
;; Modular Exponentiation: https://en.wikipedia.org/wiki/Modular_exponentiation
