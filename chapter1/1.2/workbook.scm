(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; (factorial 3) ; 6
;; (factorial 5) ; 120

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter)
		 (+ counter 1)
		 max-count)))

;; (factorial 3) ; 6
;; (factorial 5) ; 120

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

;; (fib 0) ; 0
;; (fib 1) ; 1
;; (fib 2) ; 1
;; (fib 3) ; 2
;; (fib 4) ; 3
;; (fib 5) ; 5
;; (fib 6) ; 8
;; (fib 7) ; 13
;; (fib 8) ; 21

(define (fib-iter a b n)
  (if (> n 0)
      (fib-iter (+ a b) a (- n 1))
      b))

(define (fib n)
  (fib-iter 1 0 n))

;; (fib 0) ; 0
;; (fib 1) ; 1
;; (fib 2) ; 1
;; (fib 3) ; 2
;; (fib 4) ; 3
;; (fib 5) ; 5
;; (fib 6) ; 8
;; (fib 7) ; 13
;; (fib 8) ; 21


;; Example: Counting change
;;
;; How many different ways can we make change of $ 1.00, given half-dollars,
;; quarters, dimes, nickels, and pennies?
;; More generally, can we write a procedure to compute the number of ways
;; to change any given amount of money?
;;
;; This problem has a simple solution as a recursive procedure.
;; Suppose we think of the types of coins available as arranged in some order.
;; Then the following relation holds:
;;
;; The number of ways to change amount a using n kinds of coins equals
;; - the number of ways to change amount a using all but the first kind of coin, plus
;; - the number of ways to change amount a-d using all n kinds of coins,
;;   where d is the denomination of the first kind of coin.
;;
;; Thus, we can recursively reduce the problem of changing a given amount to
;; the problem of changing smaller amounts using fewer kinds of coins.
;;
;; Considering this reduction rule, the following degenerate cases can be specified:
;; - if a is exactly 0, there is 1 way to make change
;; - if a is less than 0, there are no ways to make change
;; - if n is 0, there are no ways to make change
;;
;; This description can be translated into a recursive procedure:

(define (change a n)
  (cond ((= a 0) 1)
	((< a 0) 0)
	((= n 0) 0)
	(else (+ (change a (- n 1))
		 (change (- a (coin n)) n)))))

(define (coin i)
  (cond ((= i 1) 1)
	((= i 2) 5)
	((= i 3) 10)
	((= i 4) 25)
	((= i 5) 50)))

;; (change 10 1) ; 1 -> the number of ways to change 10 cents using 1 cent coins
;; (change 10 2) ; 3 -> the number of ways to change 10 cents using 1 and 5 cent coins
;; (change 10 3) ; 4 -> the number of ways to change 10 cents using 1, 5 and 10 cent coins

(define (change-5 a)
  (change a 5))

;; (change-5 100) ; 292 -> the number of ways to change $1 using 1, 5, 10, 25 and 50 cent coins

;; 'change' generates a tree-recursive process with redundancies similar to those
;; in the first implemnetation of 'fib'.
;;
;; The observation that a tree-recursive process may be highly inefficient but often easy
;; to specify and understand has led people to propose that one could get the best of
;; both worlds by designing a 'smart compiler' that could transform tree-recursive procedures
;; into more efficient procedures that compute the same result.


;; Exponentiation: recursive definition
;;
;; b^n = b * b^n-1
;; b^0 = 1
;;
;; This is a linear recursive process, which requires θ(n) steps and θ(n) space.

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; (expt 0 0)  ; 1 (actually, 0^0 is a math expression with no agreed-upon value)
;; (expt 0 1)  ; 0
;; (expt 1 0)  ; 1
;; (expt 1 1)  ; 1
;; (expt 1 10) ; 1
;; (expt 10 1) ; 10
;; (expt 10 3) ; 1000
;; (expt 2 10) ; 1024

;; Exponentiation: iteravite definition
;;
;; This version requires θ(n) steps and θ(1) space.

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n product)
  (if (= n 0)
      product
      (expt-iter b (- n 1) (* product b))))

;; (expt 2 3)  ; 8
;; (expt 2 10) ; 1024

;; Exponentiation: successive squaring
;;
;; Rather than computing b^8 as b*(b*(b*(b*(b*(b*(b*b))))))
;; we can compute it using three multiplications:
;;
;; b^2 = b * b
;; b^4 = b^2 * b^2
;; b^8 = b^4 * b^4
;;
;; This method works fine for exponents that are powers of 2.
;; In general, we can take advantage of successive squaring by using the rule:
;;
;; b^n = (b^n/2)^2   if n is even
;; b^n = b * b^n-1   if n is odd

(define (expt-ss b n)
  (cond ((= n 0) 1)
	((even? n) (square (expt-ss b (/ n 2))))
	(else (* b (expt-ss b (- n 1))))))

;; OR:
;; b^n = (b^2)^n/2   if n is even <- allows for tail recursion
;; b^n = b * b^n-1   if n is odd
;;
;; This version works noticeably slower on large numbers, since it recursively calls
;; itself with the current base squared, which results in unnecesary multiplications
;; of large numbers and increases the overall number of procedure calls.
;;
;; (define (expt-ss b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (expt-ss (square b) (/ n 2)))
;;         (else (* b (expt-ss b (- n 1))))))

(define (even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

;; (expt-ss 2 3)  ; 8
;; (expt-ss 2 10) ; 1024
;; (expt-ss 5 10) ; 9765625

;; The process evolved by expt-ss grows logarithmically with n in both
;; space and number of steps; it has θ(log(n)) growth.


;; Greatest Common Divisors
;; Euclidean algorithm (substraction-based)
;;
;; The Euclidean algorithm is based on the principle that the greatest common divisor
;; of two numbers does not change if the larger number is replaced by its difference
;; with the smaller number.
;; Since this replacement reduces the larger of the two numbers, repeating this process
;; gives successively smaller pairs of numbers until the two numbers become equal.
;; When that occurs, they are the GCD of the original two numbers.

(define (gcd a b)
  (cond ((> a b) (gcd (- a b) b))
	((< a b) (gcd (- b a) a))
	(else a)))

;; OR:
;;
;; (define (gcd a b)
;;  (if (= a b)
;;      a
;;      (gcd (abs (- a b)) (min a b))))

;; (gcd 16 28)   ; 4
;; (gcd 206 40)  ; 2
;; (gcd 252 105) ; 21

;; Greatest Common Divisors
;; Euclidean algorithm (division-based)
;;
;; The version of the Euclidean algorithm described above can take many subtraction steps
;; to find the GCD when one of the given numbers is much bigger than the other.
;; A more efficient version of the algorithm shortcuts these steps, instead replacing
;; the larger of the two numbers by its remainder when divided by the smaller of the two
;; (with this version, the algorithm stops when reaching a zero remainder).

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; This generates an iterative process, whose number of steps grows as the logarithm
;; of the numbers involved.

;; (gcd 16 28)   ; 4
;; (gcd 206 40)  ; 2
;; (gcd 252 105) ; 21


;; Example: Testing for Primality

;; Searching for divisors
;;
;; One way to test if a number is prime is to find the number's divisors.
;; The following program finds the smallest integral divisor (greater than 1)
;; of a given number n.
;; It does it by testing n for divisibility by successive integers starting with 2.

(define (prime? n)
  (not (div-iter n 2)))

(define (div-iter n divisor)
  (cond ((< n (square divisor)) false)
	((divisible? n divisor) true)
	(else (div-iter n (+ divisor 1)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

;; (prime? 1)  ; true
;; (prime? 2)  ; true
;; (prime? 3)  ; true
;; (prime? 4)  ; false
;; (prime? 5)  ; true
;; (prime? 6)  ; false
;; (prime? 7)  ; true
;; (prime? 8)  ; false
;; (prime? 9)  ; false
;; (prime? 10) ; false
;; (prime? 11) ; true

;; The end test for div-iter is based on the fact that if n is not prime
;; it must have a divisor less than or equal to sqrt(n):
;; if d is a divisor of n, then so is n/d. But d and n/d cannot both be greater than sqrt(n).
;;
;; This means that the algorithm needs only test divisors between 1 and sqrt(n).
;; Consequently, the number of steps required to identify n as prime
;; will have order of growth θ(sqrt(n)).


;; The Fermat test
;;
;; The θ(log(n)) primality test is based on a result from number theory
;; known as Fetmat's Little Theorem:
;; https://en.wikipedia.org/wiki/Fermat's_little_theorem
;;
;; Fermat's Little Theorem: If n is a prime number and a is any positive integer less than n,
;; then a raised to the n-th power is congruent to a modulo n.
;;
;; Two numbers are said to be congruent modulo n if they both have the same
;; remainder when divided by n.
;; The remainder of a number a when divided by n is also referred to as the remainder of
;; a modulo n, or simply as a modulo n.
;;
;; If n is not prime, then, in general, most of the numbers a<n will not satisfy the above relation.


;; A procedure that computes the exponential of a number modulo another number
;; (it uses the procedure for successive squaring implemented above):

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;; Not:
;;
;; (define (expmod base exp m)
;;   (remainder (expt-ss base exp) m))
;;
;; By making use of expt-ss, expmod calculates exponents first which greatly affects (slows down)
;; the speed of calculations involving large numbers.

;; (expmod 2 5 5) ; 2

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

;; The Fermat test differs in character from most familiar algorithms,
;; in which one computes an answer that is guaranteed to be correct.
;; Here, the answer obtained is only probably correct. More precisely,
;; if n ever fails the Fermat test, we can be certain that n is not prime.
;; But the fact that n passes the test, while an extremely strong indication,
;; is still not a guarantee that n is prime.

;; The existence of tests for which one can prove that the chance of error becomes
;; arbitrarily small has sparked interest in algorithms of this type, which have come
;; to be known as probabilistic algorithms.
;; There is a great deal of research activity in this area, and probabilistic algorithms
;; have been fruitfully applied to many fields.
