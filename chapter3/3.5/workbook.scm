;; 3.5 Streams
;;
;; In this section, we explore an alternative approach to modeling state,
;; based on data structures called 'streams'.
;; Streams can mitigate some of the complexity of modeling state.
;;
;; From an abstract point of view, a stream is simply a sequence.
;; However, the straightforward implementation of streams as lists
;; (as in section 2.2.1) doesn't fully reveal the power of stream processing.
;;
;; As an alternative, we introduce the technique of 'delayed evaluation',
;; which enables us to represent very large (even infinite) sequences
;; as streams.
;;
;; MIT Scheme - Streams:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Streams.html


;; 3.5.1 Streams Are Delayed Lists
;;
;; On the surface, streams are just lists with different names for
;; the procedures that manipulate them.
;;
;; There is a constructor, cons-stream, and two selectors, stream-car
;; and stream-cdr, which satisfy the constraints
;;
;; (stream-car (cons-stream x y)) = x
;; (stream-cdr (cons-stream x y)) = y
;;
;; For example:
;;
;; (stream-car (cons-stream 1 2)) ; 1
;; (stream-cdr (cons-stream 1 2)) ; 2
;;
;; (car (cons-stream 1 2)) ; 1
;; (cdr (cons-stream 1 2)) ; promise


;; Stream analogs of the list operations from chapter 2:

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;; See ex-3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

;; Viewing streams:

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


;; To make the stream implementation automatically and transparently
;; interleave the construction of a stream with its use,
;; we will arrange for the cdr of a stream to be evaluated when
;; it is accessed by the stream-cdr procedure rather than when
;; the stream is constructed by cons-stream.

;; Our implementation of streams will be based on a special form called 'delay'.
;;
;; Evaluating (delay <exp>) does not evaluate the expression <exp>,
;; but rather returns a so-called delayed object, which we can think of
;; as a "promise" to evaluate <exp> at some future time.
;;
;; As a companion to 'delay', there is a procedure called 'force'
;; that takes a delayed object as argument and performs the evaluation -
;; in effect, forcing the 'delay' to fulfill its promise.

;; cons-stream is a special form defined so that
;;
;; (cons-stream <a> <b>)
;;
;; is equivalent to
;;
;; (cons <a> (delay <b>))
;;
;; For example:
;; 
;; (stream-car (cons 1 (delay 2))) ; 1
;; (stream-cdr (cons 1 (delay 2))) ; 2
;; 
;; (car (cons 1 (delay 2))) ; 1
;; (cdr (cons 1 (delay 2))) ; promise


;; Rather than placing the value of the rest of the stream into the cdr
;; of the pair we will put there a promise to compute the rest if
;; it is ever requested:

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; Although stream-car and stream-cdr can be defined as procedures,
;; cons-stream must be a special form.
;;
;; If cons-stream were a procedure, then, according to our model of evaluation,
;; evaluating (cons-stream <a> <b>) would automatically cause <b> to be evaluated,
;; which is precisely what we do not want to happen.
;;
;; For the same reason, delay must be a special form, though force can be
;; an ordinary procedure.
;;
;; (force (delay (+ 1 2 3))) ; 6


;; The stream implementation in action

(load "../../common.scm")

;; Only as many integers are tested for primality as are necessary
;; to find the second prime, and the interval is enumerated only as far
;; as is necessary to feed the prime filter:
;;
;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime?
;;                  (stream-enumerate-interval 10000 1000000))))
;;
;; 10009

;; stream-enumerate-interval returns a stream represented as a pair
;; whose car is 10,000 and whose cdr is a promise to enumerate more of
;; the interval if so requested.

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

;; Stream filtering

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; Implementing delay and force
;;
;; delay must package an expression so that it can be evaluated later
;; on demand, and this can be accomplished by treating the expression
;; as the body of a procedure.
;;
;; delay can be a special form such that
;;
;; (delay <exp>)
;;
;; is syntactic sugar for
;;
;; (lambda () <exp>)
;;
;; force simply calls the procedure (of no arguments) produced by delay:
;;
;; (define (force delayed-object)
;;   (delayed-object))

;; There is an important optimization that can be included.
;;
;; In many applications, we end up forcing the same delayed object
;; many times. This can lead to serious inefficiency in recursive
;; programs involving streams.
;;
;; The solution is to build delayed objects so that the first time
;; they are forced, they store the value that is computed.
;; Subsequent forcings will simply return the stored value without
;; repeating the computation.
;;
;; One way to accomplish this is to use the following procedure,
;; which takes as argument a procedure (of no arguments) and returns
;; a memoized version of the procedure.
;;
;; The first time the memoized procedure is run,
;; it saves the computed result. On subsequent evaluations,
;; it simply returns the result.

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; Test: memo-proc
;;
;; (define (proc-1)
;;   (newline)
;;   (display "proc1 calculations are running...")
;;   99)
;;
;; (define memo-proc-1 (memo-proc proc-1))
;;
;; (memo-proc-1) ; calculations are running... 99
;; (memo-proc-1) ; 99


;; delay is then defined so that (delay <exp>) is equivalent to
;;
;; (memo-proc (lambda () <exp>))
;;
;; and force is as defined previously.
;;


;; Delayed evaluation, which is the key to making streams practical,
;; was inherent in Algol 60's 'call-by-name' parameter-passing method.
;;
;; The memoizing optimization is also known as 'call-by-need'.


;; 3.5.2 Infinite Streams
;;
;; We can use streams to represent sequences that are infinitely long.
;;
;; For instance, consider the following definition of the stream of
;; positive integers:

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; integers ; {1 ...}

;; This is an infinitely long stream, but in any given time
;; we can examine only a finite portion of it.
;; Thus, our programs will never know that the entire infinite stream
;; is not there.

;; Using integers we can define other infinite streams.
;;
;; The stream of integers that are not divisible by 7:

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; (stream-ref no-sevens 100) ; 117


;; The infinite stream of Fibonacci numbers:

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;; (stream-ref fibs 5) ; 5
;; (stream-ref fibs 6) ; 8
;; (stream-ref fibs 7) ; 13


;; The infinite stream of prime numbers.
;; Method: the sieve of Eratosthenes.
;;
;; To sieve a stream S, form a stream whose first element
;; is the first element of S and the rest of which is obtained by
;; filtering all multiples of the first elemen of S out of
;; the rest of S and sieving the result.

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;; (stream-ref primes 50) ; 233

;; See 'Henderson diagrams', figures named after Peter Henderson,
;; where the prime sieve is viewed as a signal-processing system.


;; Defining streams implicitly
;;
;; An alternative way to specify streams is to take advantage of
;; delayed evaluation to define streams implicitly.
;;
;; The following expression defines the stream 'ones' to be
;; an infinite stream of ones:

(define ones (cons-stream 1 ones))

;; 'ones' is a pair whose car is 1 and whose cdr is a promise
;; to evaluate 'ones'.
;; Evaluating the cdr gives us again a 1 and a promise
;; to evaluate 'ones', and so on.


;; Produce the elementwise sum of two given streams:

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Now we can define the integers as follows:

(define integers (cons-stream 1 (add-streams ones integers)))

;; The Fibonacci numbers defined implicitly:

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;; This definition says that fibs is a stream beginning with 0 and 1,
;; such that the rest of the stream can be generated by
;; adding fibs to itself shifted by one place:
;;
;;     1 1 2 3 5 8  13 21 ... = (stream-cdr fibs)
;;     0 1 1 2 3 5  8  13 ... = fibs
;; 0 1 1 2 3 5 8 13 21 34 ... = fibs


;; Scaling streams: multiply each item in a stream by a given constant:

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; Produce the stream of powers of 2:

(define double (cons-stream 1 (scale-stream double 2)))

;; (stream-ref double 0) ; 1
;; (stream-ref double 1) ; 2
;; (stream-ref double 2) ; 4
;; (stream-ref double 3) ; 8
;; (stream-ref double 4) ; 16


;; An alternative definition of the stream of primes.
;;
;; Start with the integers and filter them by testing for primality.

(define primes
  (cons-stream
   2
   (stream-filter s-prime? (integers-starting-from 3))))

(define (s-prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;; (stream-ref primes 50) ; 233

;; This is a recursive definition, since primes is defined in terms of
;; the prime? predicate, which itself uses the primes stream.
;;
;; The reason this procedure works is that, at any point,
;; enough of the primes stream has been generated to test
;; the primality of the numbers we need to check next.
;;
;; That is, for every n we test for primality, either n is not prime
;; (in which case there is a prime already generated that divides it)
;; or n is prime (in which case there is a prime already generated -
;; - i.e., a prime less that n - that is greater than sqrt(n)).


;; 3.5.3 Exploiting the Stream Paradigm
;;
;; Streams with delayed evaluation can be a powerful modeling tool,
;; providing many of the benefits of local state and assignment.


;; Formulating iterations as stream processes
;;
;; We can represent state as a 'timeless' stream of values
;; rather than as a set of variables to be updated.


;; Square root guesses
;;
;; Recall the idea to generate a sequence of better and better guesses
;; for the square root of x by applying over and over again
;; the procedure that improves guesses (from section 1.1.7):

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

;; In the original 'sqrt' procedure, these guesses are made
;; be the successive values of a state variable.
;;
;; Instead we can generate the infinite stream of guesses,
;; starting with an initial guess of 1:

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; (define sqrt-of-2 (sqrt-stream 2))
;;
;; (stream-ref sqrt-of-2 0) ; 1.
;; (stream-ref sqrt-of-2 1) ; 1.5
;; (stream-ref sqrt-of-2 2) ; 1.4166666666666665
;; (stream-ref sqrt-of-2 3) ; 1.4142156862745097
;; (stream-ref sqrt-of-2 4) ; 1.4142135623746899


;; An approximation to pi
;;
;; pi/4  = 1 - 1/3 + 1/5 - 1/7 + ...
;;
;; We first generate the stream of summands of the series
;; (the reciprocals of the odd integers, with alternating signs).
;;
;; Then we take the stream of sums of more and more terms
;; (using the parital-sums procedure of ex. 3.55) and
;; scale the result by 4.

;; See ex-3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; (stream-ref pi-stream 0) ; 4.
;; (stream-ref pi-stream 1) ; 2.666666666666667
;; (stream-ref pi-stream 2) ; 3.466666666666667
;; (stream-ref pi-stream 3) ; 2.8952380952380956
;; (stream-ref pi-stream 4) ; 3.3396825396825403
;; (stream-ref pi-stream 5) ; 2.9760461760461765
;; (stream-ref pi-stream 6) ; 3.2837384837384844
;; (stream-ref pi-stream 7) ; 3.017071817071818


;; Sequence accelerators
;;
;; A stream can be transformed with a 'sequence accelerator' that
;; converts a sequence of approximations to a new sequence that
;; converges to the same value as the original, only faster.

;; Euler's technique.
;;
;; The technique works well with the sequences that are partial sums
;; of alternating series (series of terms with alternating signs).
;;
;; If S(n) is the n-th term of the original sum sequence, then
;; the accelerated sequence has terms
;;
;; S(n+1) - (S(n+1) - S(n))^2 / (S(n-1) - 2S(n) + S(n+1))
;;
;; Thus, if the original sequence is represented as a stream of values,
;; the transformed sequence is given by

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S(n-1)
        (s1 (stream-ref s 1))  ; S(n)
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; Euler acceleration with the sequence of approximations to pi:

(define euler-pi-stream (euler-transform pi-stream))

;; (stream-ref euler-pi-stream 0) ; 3.166666666666667
;; (stream-ref euler-pi-stream 1) ; 3.1333333333333337
;; (stream-ref euler-pi-stream 2) ; 3.1452380952380956
;; (stream-ref euler-pi-stream 3) ; 3.13968253968254
;; (stream-ref euler-pi-stream 4) ; 3.1427128427128435
;; (stream-ref euler-pi-stream 5) ; 3.1408813408813416
;; (stream-ref euler-pi-stream 6) ; 3.142071817071818
;; (stream-ref euler-pi-stream 7) ; 3.1412548236077655


;; Even better, we can accelerate the accelerated sequence,
;; and recursively accelerate that, and so on.
;;
;; Namely, we create a stream of streams (a structure we'll call a 'tableau')
;; in which each stream is the transform of the preceding one:

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

;; The tableau has the form
;;
;; s00 s01 s02 s03 s04 ...
;;     s10 s11 s12 s13 ...
;;         s20 s21 s22 ...
;;                 ...

;; Finally, we form a sequence by taking the first term
;; in each row of the tableau:

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


;; Tableau-based acceleration of the pi-sequence:

(define accel-pi-stream
  (accelerated-sequence euler-transform pi-stream))

;; (stream-ref accel-pi-stream 0) ; 4.
;; (stream-ref accel-pi-stream 1) ; 3.166666666666667
;; (stream-ref accel-pi-stream 2) ; 3.142105263157895
;; (stream-ref accel-pi-stream 3) ; 3.141599357319005
;; (stream-ref accel-pi-stream 4) ; 3.1415927140337785
;; (stream-ref accel-pi-stream 5) ; 3.1415926539752927
;; (stream-ref accel-pi-stream 6) ; 3.1415926535911765
;; (stream-ref accel-pi-stream 7) ; 3.141592653589778

;; These acceleration techniques could have been implemented
;; without using streams.
;;
;; But the stream formulation is particularly elegant and convenient
;; because the entire sequence of states is available to us as
;; a data structure that can be manipulated with a uniform
;; set of operations.


;; Infinite streams of pairs
;;
;; As shown in section 2.2.3, the sequence paradigm can handle
;; traditional nested loops as processes defined on sequences of pairs.
;;
;; If we generalize this technique to infinite streams, then we can
;; write programs that are not easily represented as loops,
;; because the "looping" must range over an infinite set.

;; Generalization of the primi-sum-pairs procedure of section 2.2.3
;;
;; The goal is to produce the stream of pairs of all integers (i,j)
;; with i <= j such that i + j is prime.
;;
;; If int-pairs is the sequence of all pairs of integers (i,j)
;; with i <= j, then the required stream is simply
;;
;; (stream-filter (lambda (pair)
;;                  (prime? (+ (car pair) (cadr pair))))
;;                int-pairs)

;; Now, the problem is to produce the stream int-pairs.
;;
;; Suppose we have two streams S = (S(i)) and T = (T(j)),
;; and imagine the infinite rectangular array
;;
;; (S0,T0) (S0,T1) (S0,T2) ...
;; (S1,T0) (S1,T1) (S1,T2) ...
;; (S2,T0) (S2,T1) (S2,T2) ...
;; ...
;;
;; The need is to generate a stream that contains all the pairs in the array
;; that lie on or above the diagonal, i.e., the pairs
;;
;; (S0,T0) (S0,T1) (S0,T2) ...
;;         (S1,T1) (S1,T2) ...
;;                 (S2,T2) ...
;;                         ...
;;
;; The resulting stream is to be composed of three parts:
;; 1 - the pair (S0,T0)
;; 2 - the rest of the pairs in the first row,
;; 3 - the remaining pairs
;;
;; (S0,T0) | (S0,T1) (S0,T2) ...
;; --------|--------------------
;;         | (S1,T1) (S1,T2) ...
;;         |         (S2,T2) ...
;;         |                 ...
;;
;; Hence, the 1st piece in this decomposition is the pair (S0,T0);
;;        the 2nd piece is
;;
;; (stream-map (lambda (x) (list (stream-car s) x))
;;             (stream-cdr t))
;;
;; and    the 3rd piece (pairs that are not in the 1st row) is (recursively)
;;        the pairs formed from (stream-cdr S) and (stream-cdr T).

;; Thus the stream of pairs can be formed as follows:

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (<combine-in-some-way>
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Combine the two inner (infinite) streams.
;;
;; One idea is to use the stream analog of the append procedure
;; (available as the build-in stream-append procedure):
;;
;; (define (stream-append s1 s2)
;;   (if (stream-null? s1)
;;       s2
;;       (cons-stream (stream-car s1)
;;                    (stream-append (stream-cdr s1) s2))))
;;
;; However, this is unsuitable for infinite streams, because
;; it takes all the elements from the first stream before
;; incorporating the second stream.
;;
;; To handle infinite streams, the order of combination must ensure
;; that every element will eventually be reached if we let
;; the program run long enough.
;;
;; The following procedure does the job:

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; Since interleave takes elements alternately from the two streams,
;; every element of the second stream will eventually find its way
;; into the interleaved stream, even if the first stream is infinite.
;;
;; Thus, the required stream of pairs can be generated as

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; (define int-pairs (pairs integers integers))
;;
;; (stream-ref int-pairs 0) ; (1 1)
;; (stream-ref int-pairs 1) ; (1 2)
;; (stream-ref int-pairs 2) ; (2 2)
;; (stream-ref int-pairs 3) ; (1 3)
;; (stream-ref int-pairs 4) ; (2 3)
;; (stream-ref int-pairs 5) ; (1 4)
;; (stream-ref int-pairs 6) ; (3 3)
;; (stream-ref int-pairs 7) ; (1 5)
;; (stream-ref int-pairs 8) ; (2 4)
;; (stream-ref int-pairs 9) ; (1 6)


;; Streams as signals
;;
;; Streams can be used to model signal-processing systems in a very direct way,
;; representing the values of a signal at successive time intervals as
;; consecutive elements of a stream.

;; Integrator (summer)
;;
;; For an input stream x = (x(i)), an initial value C, and
;; a small increment dt, an integrator accumulates the sum
;;
;;             i
;; S(i) = C +  Σ (x(j) * dt)
;;            j=1
;;
;; and returns the stream of values S = (S(i)).

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


;; 3.5.4 Streams and Delayed Evaluation
;;
;; In general, `delay` is crucial for using streams to model
;; signal-processing systems that contain loops.
;;
;; Without `delay`, our models would have to be formulated so that
;; the inputs to any signal-processing component would be fully evaluated
;; before the output could be produced. This would outlaw loops.

;; Stream models of systems with loops may require uses of delay
;; beyond the "hidden" delay supplied by cons-stream.

;; Redefined integral that expects the integrand stream to be
;; a delayed argument. Integral will force the integrand
;; to be evaluated only when it is required to generate more
;; than the first element of the output stream:

(define (integral-delayed delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

;; An "analog computer circuit" that solves the equation dy/dt = f(y)
;;
;; Every caller of integral must now delay the integrand argument.

(define (solve f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; The procedure below does not work, because in the first line of solve
;; the call to integral requires that the input dy be defined,
;; which does not happen until the second line of solve:
;;
;; (define (solve f y0 dt)
;;   (define y (integral dy y0 dt))
;;   (define dy (stream-map f y))
;;   y)

;; Approximating e ~ 2.718
;;
;; (stream-ref (solve (lambda (y) y) 1 0.001) 1000) ; 2.716923932235896

;; In effect, we have created two classes of procedures:
;; ordinary procedures and procedures that take delayed arguments.
;;
;; In general, creating separate classes of procedures forces us to
;; create separate classes of higher-order procedures as well.


;; 3.5.5 Modularity of Functional Programs and Modularity of Objects
;;
;; One of the major benefits of introducing assignment is that
;; we can increase the modularity of our systems by encapsulating,
;; or "hiding", parts of the state of a large system
;; within local variables.
;;
;; Stream models can provide an equivalent modularity without
;; the use of assignment.


;; The Monte Carlo estimation of pi from a stream-processing pov
;;
;; (examined in section 3.1.2)
;;
;; The key modularity issue was that we wished to hide
;; the internal state of a random-number generator
;; from programs that used random numbers.
;;
;; In the stream formulation there is no random-number generator per se,
;; just a stream of random numbers produced by successive calls
;; to rand-update:

(load "../ch3support.scm")

(define random-init (random 100))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

;; (stream-ref random-numbers 0) ; 4
;; (stream-ref random-numbers 1) ; 7
;; (stream-ref random-numbers 2) ; 88
;; (stream-ref random-numbers 3) ; 116
;; (stream-ref random-numbers 4) ; 110
;; (stream-ref random-numbers 5) ; 75

;; Construct the stream of outcomes of the Cesaro experiment
;; performed on consecutive pairs in the random-numbers stream:

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

;; The cesaro-stream is fed to a monte-carlo procedure,
;; which produces a stream of estimates of probabilities.
;;
;; The results are then converted into a stream of estimates of pi.
;;
;; This version of the program doesn't need a parameter telling
;; how many trials to perform.
;;
;; Better estimates of pi (from performing more experiments)
;; are obtained by looking farther into the pi stream:

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; (define pi
;;   (stream-map (lambda (p) (sqrt (/ 6 p))) ;; Check for p = 0
;;               (monte-carlo cesaro-stream 0 0)))

;; (stream-ref pi 0) ; 2.449489742783178
;; (stream-ref pi 1) ; 3.4641016151377544
;; (stream-ref pi 2) ; 4.242640687119285
;; (stream-ref pi 3) ; 3.4641016151377544
;; (stream-ref pi 4) ; 3.1622776601683795
;; (stream-ref pi 5) ; 3
;; (stream-ref pi 6) ; 3.24037034920393
;; (stream-ref pi 7) ; 3.0983866769659336


;; A functional-programming view of time

;; Withdrawal processor (local state, section 3.1.3).

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))


;; Withdrawal processor (streams)
;;
;; Stream-withdraw implements a well-defined mathematical function
;; whose output is fully determined by its input.

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))


;; From the perspective of the user who is typing values and
;; watching results, the stream process has the same behavior
;; as the object created by make-simplified-withdraw.
;;
;; However, with the stream version, there is no assignment,
;; no local state variable, and consequently none of the theoretical
;; difficulties that we encountered in section 3.1.3.
;;
;; Yet the system has state!

;; Even though stream-withdraw implements a well-defined
;; mathematical function whose behavior does not change,
;; the user's perception here is one of interacting with a system
;; that has a changing state.
;;
;; One way to resolve this paradox is to realize that it is
;; the user's temporal existence that imposes state on the system.
;;
;; If the user could step back from the interaction and think
;; in terms of streams of balances rather than individual transactions,
;; the system would appear stateless.
