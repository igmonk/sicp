;; Exercise 1.22
;;
;; Most Lisp implementations include a primitive called 'runtime' that returns an integer that
;; specifies the amount of time the system has been running (measured, for example, in microseconds).
;;
;; The following timed-prime-test procedure, when called with an integer n, prints n and checks
;; to see if n is prime. If n is prime, the procedure prints three asterisks
;; followed by the amount of time used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
	 (report-prime (- (runtime) start-time))
	 true)
	(else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; The already familiar implementation of the prime? function is given below:

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((< n (square divisor)) n)
	((divisible? n divisor) divisor)
	(else (find-divisor n (+ divisor 1)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

(define (square x)
  (* x x))

;; Using the report-time procedure, write a procedure search-for-primes that checks the primality
;; of consecutive odd integers in a specified range.
;; Use your procedure to find the three smallest primes larger than 1000; larger than 10,000;
;; larger than 100,000; larger than 1,000,000.
;; Note the time needed to test each prime. Since the testing algorithm has order of growth
;; of θ(sqrt(n)), you should expect that testing for primes around 10,000 should take about
;; sqrt(10) times as long as testing for primes around 1000.
;; Do your timing data bear this out?
;; How well do the data for 100,000 and 1,000,000 support the n prediction?
;; Is your result compatible with the notion that programs on your machine run in time
;; proportional to the number of steps required for the computation?

(define (search-for-primes from to)
  (if (<= from to)
      (cond ((odd? from)
	     (timed-prime-test from)
	     (search-for-primes (+ from 2) to))
	    (else (search-for-primes (+ from 1) to)))))

(define (search-for-primes from to count)
  (cond ((or (> from to) (= count 0)))
	((even? from) (search-for-primes (+ from 1) to count))
	(else (if (timed-prime-test from)
		  (search-for-primes (+ from 2) to (- count 1))
		  (search-for-primes (+ from 2) to count)))))

;; (search-for-primes 1000 1100 3)
;; 1009 *** 0.
;; 1013 *** 0.
;; 1019 *** 0.

;; (search-for-primes 10000 11000 3)
;; 10007 *** 0.
;; 10009 *** 0.
;; 10037 *** 0.

;; (search-for-primes 100000 110000 3)
;; 100003 *** 0.
;; 100019 *** 0.
;; 100043 *** 0.

;; (search-for-primes 1000000 1100000 3)
;; 1000003 *** 0.
;; 1000033 *** 0.
;; 1000037 *** 0.

;; (search-for-primes 10000000 11000000 3)
;; 10000019 *** 1.0000000000000009e-2
;; 10000079 *** 0.
;; 10000103 *** 0.

;; (search-for-primes 100000000 110000000 3)
;; 100000007 *** 1.0000000000000009e-2
;; 100000037 *** .01999999999999999
;; 100000039 *** 1.0000000000000009e-2

;; (search-for-primes 1000000000 1100000000 3)
;; 1000000007 *** .03
;; 1000000009 *** 2.0000000000000018e-2
;; 1000000021 *** .02999999999999997

;; (search-for-primes 10000000000 11000000000 3)
;; 10000000019 *** .08000000000000002
;; 10000000033 *** .09000000000000002
;; 10000000061 *** .07999999999999996

;; (search-for-primes 100000000000 110000000000 3)
;; 100000000003 *** .27
;; 100000000019 *** .2699999999999999
;; 100000000057 *** .26

;; (search-for-primes 1000000000000 1100000000000 3)
;; 1000000000039 *** .8099999999999998
;; 1000000000061 *** .8199999999999998
;; 1000000000063 *** .8200000000000003

;; (search-for-primes 10000000000000 11000000000000 3)
;; 10000000000037 *** 2.55
;; 10000000000051 *** 2.6100000000000003
;; 10000000000099 *** 2.5700000000000003

;; (search-for-primes 100000000000000 110000000000000 3)
;; 100000000000031 *** 8.149999999999999
;; 100000000000067 *** 8.280000000000001
;; 100000000000097 *** 8.25

;; Given the test runs above, where each subsequent input grows 10 times its previous or,
;; in other words, each second input grows 100 times its pre-previous, the following is observed:
;; The prime 100000000000031 took 8.14999999999999900 seconds to find, whereas
;; the prime   1000000000039 took  .80999999999999990 seconds to find, whereas
;; the prime     10000000019 took  .08000000000000002 seconds to find, which means
;; each time the input grew 100 times, the required time to find a prime increased
;; approximately sqrt(100) = 10 times, which corresponds to the expected prediction.
