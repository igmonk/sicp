;; Exercise 1.19
;;
;; There is a clever algorithm for computing the Fibonacci numbers
;; in a logarithmic number of steps.
;;
;; Recall the transformation of the state variables a and b
;; in the fib-iter process of section 1.2.2:
;; a <- a + b
;; b <- a
;;
;; Call this transformation T, and observe that applying T over and over again
;; n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n).
;; In other words, the Fibonacci numbers are produced by applying T^n,
;; the nth power of the transformation T, starting with the pair (1,0).
;;
;; Now consider T to be the special case of p=0 and q=1 in a family of
;; transformations T<pq>, where T<pq> transforms the pair (a,b) according to
;; a <- bq + aq + ap
;; b <- bp + aq
;;
;; Show that if we apply such a transformation T<pq> twice,
;; the effect is the same as using a single transformation T<p'q'> of the same form,
;; and compute p' and q' in terms of p and q.
;; This gives us an explicit way to square these transformations, and thus we can
;; compute T^n using successive squaring, as in the fast-expt procedure.
;;
;; Put this all together to complete the following procedure,
;; which runs in a logarithmic number of steps.


;; Solution
;;
;; The derivation of an θ(log(n)) program for Fibonacci is based on a rather
;; general technique exploiting the fact that the expressions assigned to x and y
;; in the multiple assignment x,y := y,x+y are linear combinations of x and y.
;;
;; In terms of matrices this assignment is denoted as
;;
;; |x| := |0 1| * |x|
;; |y|    |1 1|   |y|
;;
;; In the algorithm, an invariant of the program for Fibonacci is
;;
;; |x| := |0 1|^n * |x|
;; |y|    |1 1|     |y|
;;
;; and its post-condition is
;;
;; |x| := |0 1|^N * |x|
;; |y|    |1 1|     |y|
;;
;; Taking advantage of successive squaring leads to:
;;
;; A^n := (A^2)^n/2   if n is even
;; A^n := A * A^n-1   if n is odd
;;
;; where A is initialized to |0 1| and A^n-1 corresponds to |x|
;;                           |1 1|                          |y|
;;
;; Computing some powers of A leads to the conjecture that all these powers are
;; of the form |a  b |
;;             |b a+b|
;;
;; |a  b | * |a  b | = |p  q |
;; |b a+b|   |b a+b|   |q p+q|
;;
;; where p = a^2 + b^2 and q = ab + ba + b^2
;;
;; Hence, matrix A may be represented by two integers:
;; pair <a,b> represents matrix |a  b |
;;                              |b a+b|
;;
;; Then A^2 corresponds to <a^2 + b^2, ab + ba + b^2>
;; and A * A^n-1  translates to A * |x| and corresponds to <ax + by, bx + ay + by>
;;                                  |y|
;;
;; Source: The Derivation of Algorithms by A. Kaldewaij.
;; Section: 5.2 Fibonacci

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count) (fib-iter a
				 b
				 (+ (* p p) (* q q))         ;; a^2 + b^2
				 (+ (* p q) (* q p) (* q q)) ;; ab + ba + b^2
				 (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;; (fib 0) ; 0
;; (fib 1) ; 1
;; (fib 2) ; 1
;; (fib 3) ; 2
;; (fib 4) ; 3
;; (fib 5) ; 5
;; (fib 6) ; 8
;; (fib 7) ; 13
;; (fib 8) ; 21
;; (fib 9) ; 34
