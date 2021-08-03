;; Exercise 1.23
;;
;; The smallest-divisor procedure shown at the start of this section does lots of needless testing:
;; After it checks to see if the number is divisible by 2 there is no point in checking to see if
;; it is divisible by any larger even numbers.
;; This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but rather
;; 2, 3, 5, 7, 9, ....
;; To implement this change, define a procedure 'next' that returns 3 if its input is equal to 2
;; and otherwise returns its input plus 2.
;; Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).
;; With timed-prime-test incorporating this modified version of smallest-divisor,
;; run the test for each of the 12 primes found in exercise 1.22.
;; Since this modification halves the number of test steps, you should expect it
;; to run about twice as fast. Is this expectation confirmed?
;; If not, what is the observed ratio of the speeds of the two algorithms,
;; and how do you explain the fact that it is different from 2?

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

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((< n (square divisor)) n)
	((divisible? n divisor) divisor)
	(else (find-divisor n (next divisor)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

(define (square x)
  (* x x))

;; (timed-prime-test 1009) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 1013) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 1019) ; 0. (ex. 1.22: 0.)

;; (timed-prime-test 10007) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 10009) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 10037) ; 0. (ex. 1.22: 0.)

;; (timed-prime-test 100003) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 100019) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 100043) ; 0. (ex. 1.22: 0.)

;; (timed-prime-test 1000003) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 1000033) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 1000037) ; 0. (ex. 1.22: 0.)

;; (timed-prime-test 10000019) ; 9.999999999990905e-3 (ex. 1.22: 1.0000000000000009e-2)
;; (timed-prime-test 10000079) ; 0. (ex. 1.22: 0.)
;; (timed-prime-test 10000103) ; 0. (ex. 1.22: 0.)

;; (timed-prime-test 100000007) ; 1.0000000000005116e-2 (ex. 1.22: 1.0000000000000009e-2)
;; (timed-prime-test 100000037) ; 1.0000000000005116e-2 (ex. 1.22: .01999999999999999)
;; (timed-prime-test 100000039) ; 0. (ex. 1.22: 1.0000000000000009e-2)

;; (timed-prime-test 1000000007) ; .01999999999999602 (ex. 1.22: .03)
;; (timed-prime-test 1000000009) ; .01999999999999602 (ex. 1.22: 2.0000000000000018e-2)
;; (timed-prime-test 1000000021) ; .01999999999999602 (ex. 1.22: .02999999999999997)

;; (timed-prime-test 10000000019) ; 6.0000000000002274e-2 (ex. 1.22: .08000000000000002)
;; (timed-prime-test 10000000033) ; .04999999999999716 (ex. 1.22: .09000000000000002)
;; (timed-prime-test 10000000061) ; 6.0000000000002274e-2 (ex. 1.22: .07999999999999996)

;; (timed-prime-test 100000000003) ; .1599999999999966 (ex. 1.22: .27)
;; (timed-prime-test 100000000019) ; .1700000000000017 (ex. 1.22: .2699999999999999)
;; (timed-prime-test 100000000057) ; .20000000000000284 (ex. 1.22: .26)

;; (timed-prime-test 1000000000039) ; .5100000000000051 (ex. 1.22:. 8099999999999998)
;; (timed-prime-test 1000000000061) ; .5 (ex. 1.22: .8199999999999998)
;; (timed-prime-test 1000000000063) ; .5 (ex. 1.22: .8200000000000003)

;; (timed-prime-test 10000000000037) ; 1.6500000000000057 (ex. 1.22:  2.55)
;; (timed-prime-test 10000000000051) ; 1.5699999999999932 (ex. 1.22: 2.6100000000000003)
;; (timed-prime-test 10000000000099) ; 1.5600000000000023 (ex. 1.22: 2.5700000000000003)

;; (timed-prime-test 100000000000031) ; 5.049999999999997 (ex. 1.22: 8.149999999999999)
;; (timed-prime-test 100000000000067) ; 4.980000000000004 (ex. 1.22: 8.280000000000001)
;; (timed-prime-test 100000000000097) ; 4.959999999999994 (ex. 1.22: 8.25)

;; The expectation of the ratio of the speeds of the two algorithms is 2, since any
;; even numbers larger than 2 are not used during the primality test.
;;
;; Given the test runs above, the following could be ovserved:
;; the bigger input under test, the closer the time difference to 2x in comparison to
;; the initial implementation of smallest-divisor.
;;
;; The reason for the discrepancy between what is expected and the actual state of affairs
;; could be the added complexity introduced by the 'next' function:
;; besides adding one more frame on top of the call stack,
;; it uses an additional operator - 'if'.
