;; Exercise 1.24
;;
;; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method),
;; and test each of the 12 primes you found in that exercise.
;; Since the Fermat test has (log n) growth, how would you expect the time to test primes
;; near 1,000,000 to compare with the time needed to test primes near 1000?
;; Do your data bear this out? Can you explain any discrepancy you find?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100)
	 (report-prime (- (runtime) start-time))
	 true)
	(else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; The already familiar implementations of the expmod and fermat-test functions are given below:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))

;; The Fermat test is performed by choosing at random a number a between 1 and n-1 inclusive
;; and checking whether the remainder modulo n of the n-th power of a is equal a.

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; The following procedure runs the test a given number of times, as specified by a parameter.
;; Its value is true if the test succeeds every time, and false otherwise.

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; (timed-prime-test 1009) ; 0.
;; (timed-prime-test 1013) ; 0.
;; (timed-prime-test 1019) ; 0.

;; (timed-prime-test 10007) ; 0.
;; (timed-prime-test 10009) ; 0.
;; (timed-prime-test 10037) ; 0.

;; (timed-prime-test 100003) ; 0.
;; (timed-prime-test 100019) ; 0.
;; (timed-prime-test 100043) ; 0.

;; (timed-prime-test 1000003) ; 0.
;; (timed-prime-test 1000033) ; 0.
;; (timed-prime-test 1000037) ; 9.999999999990905e-3

;; (timed-prime-test 10000019) ; 0.
;; (timed-prime-test 10000079) ; 0.
;; (timed-prime-test 10000103) ; 0.

;; (timed-prime-test 100000007) ; 1.0000000000005116e-2
;; (timed-prime-test 100000037) ; 9.999999999990905e-3
;; (timed-prime-test 100000039) ; 0.

;; (timed-prime-test 1000000007) ; 9.999999999990905e-3
;; (timed-prime-test 1000000009) ; 1.0000000000005116e-2
;; (timed-prime-test 1000000021) ; 0.

;; (timed-prime-test 10000000019) ; 1.0000000000005116e-2
;; (timed-prime-test 10000000033) ; 9.999999999990905e-3
;; (timed-prime-test 10000000061) ; 1.0000000000005116e-2

;; (timed-prime-test 100000000003) ; 1.0000000000005116e-2
;; (timed-prime-test 100000000019) ; 1.0000000000005116e-2
;; (timed-prime-test 100000000057) ; 9.999999999990905e-3

;; (timed-prime-test 1000000000039) ; 1.0000000000005116e-2
;; (timed-prime-test 1000000000061) ; 9.999999999990905e-3
;; (timed-prime-test 1000000000063) ; 1.0000000000005116e-2

;; (timed-prime-test 10000000000037) ; 9.999999999990905e-3
;; (timed-prime-test 10000000000051) ; 1.0000000000005116e-2
;; (timed-prime-test 10000000000099) ; 9.999999999990905e-3

;; (timed-prime-test 100000000000031) ; 1.0000000000005116e-2
;; (timed-prime-test 100000000000067) ; 9.999999999990905e-3
;; (timed-prime-test 100000000000097) ; 1.0000000000005116e-2

;; It is worth mentioning the number of Fermat tests required to assert a number is prime
;; greatly affects execution time of the algorithm, since the method is probabilistic in nature.
;; On average, this means θ(n) growth in time, where n is the number of Fermat tests
;; performed against its input - an allegedly prime number.
;;
;; Therefore, keeping the number of Fermat tests the same for different inputs is crucial
;; for a proper evaluation of growth.
;;
;; Since the Fermat test has θ(log(n)) growth, the time to test primes near 10^6
;; is expected to be log(10^6)/log(10^3) = 6/3 = 2 times longer in comparison to
;; that of primes near 10^3.
;;
;; The test runs above were run with the number of Fermat tests set to 100.
;; As sees from their execution time, it takes very little time to define (with
;; some degree of probability) whether a given number is prime, no matter how
;; large the number is, which is not enough to investigate the algorithm growth.
;;
;; By tweaking the number of Fermat tests, from 100 to 1000, 10000, and 100000,
;; a better evaluation of the growth can be done: 
;;
;; (define (start-prime-test n start-time)
;;   (cond ((fast-prime? n 100000) ;; <- try 1000, 1000, and 100000
;;         (report-prime (- (runtime) start-time))
;;         true)
;;         (else false)))

;; The number of Fermat tests is 1000
;;
;; (timed-prime-test 10000000061)     ; 6.0000000000002274e-2
;; (timed-prime-test 100000000057)    ; .0799999999999983
;; (timed-prime-test 1000000000063)   ; .07000000000000739
;; (timed-prime-test 10000000000099)  ; .0799999999999983
;; (timed-prime-test 100000000000097) ; .09000000000000341

;; The number of Fermat tests is 10000
;;
;; (timed-prime-test 10000000061)     ; .5999999999999943
;; (timed-prime-test 100000000057)    ; .6599999999999966
;; (timed-prime-test 1000000000063)   ; .6899999999999977
;; (timed-prime-test 10000000000099)  ; .730000000000004
;; (timed-prime-test 100000000000097) ; .7800000000000011

;; The number of Fermat tests is 100000
;;
;; (timed-prime-test 10000000061)     ; 5.859999999999999
;; (timed-prime-test 100000000057)    ; 6.570000000000007
;; (timed-prime-test 1000000000063)   ; 6.939999999999998
;; (timed-prime-test 10000000000099)  ; 7.319999999999993
;; (timed-prime-test 100000000000097) ; 7.840000000000003

;; The additional tests above show almoost linear growth across the groups with
;; the number of Fermat tests growing from 1000 to 10000 to 100000.
;;
;; From within the group with the same number of Fermat tests the growth resembles
;; a logarithmic function. Considering the last group with 100000 as the number of
;; Fermat tests, the input increases 10 times with each subsequent function call,
;; whereas the elapsed time grows linearly.
