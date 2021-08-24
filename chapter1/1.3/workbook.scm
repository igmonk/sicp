;; 1.3.1 Procedures as Arguments

;; Procedure computes the sum of the integers from a through b:

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;; (sum-integers 1 10) ; 55

;; Procedure computes the sum of the cubes of the integers in the given range:

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;; (sum-cubes 1 10) ; 3025

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

;; Procedure computes the sum of a sequence of terms in the series
;;
;; 1/1*3 + 1/5*7 + 1/9*11 + ...
;;
;; which converges to pi/8:
;;
;; Odd numbers: 2n-1
;;
;; 1: (2n-1 2n) -> (1 2) -> 1: 2n-1 -> 1
;;                          2: 2n-1 -> 3
;;
;; 2: (2n-1 2n) -> (3 4) -> 3: 2n-1 -> 5
;;                          4: 2n-1 -> 7
;;
;; 3: (2n-1 2n) -> (5 6) -> 5: 2n-1 -> 9
;;                          6: 2n-1 -> 11
;; ...
;;
;; n: (2n-1 2n) -> 2n-1: 2n-1 -> 2(2n-1)-1 = 4n-3
;;                   2n: 2n-1 -> 2(2n)-1   = 4n-1

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* (- (* 4 a) 3)
		   (- (* 4 a) 1)))
	 (pi-sum (+ a 1) b))))

;; (* (pi-sum 1 1000) 8) ; 3.141092653621043

;; These three procedures (sum-integers, sum-cubes and pi-sum) share a common underlying pattern.
;; They are for the most part identical, differing only in:
;; - the name of the procedure
;; - the function of a used to compute the term to be added
;; - the function that provides the next value of a
;;
;; We could generate each of the procedures by filling in slots in the same template:

;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;;          (<name> (<next> a) b))))

;; As program designers, we would like our language to be powerful enough so that
;; we can write a procedure that expresses the concept of summation itself rather than
;; only procedures that compute particular sums.
;;
;; It can be done by taking the common template shown above and transforming
;; the 'slots' into formal parameters:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; Now sum takes as its arguments the lower and upper bounds a anb b together with
;; the procedures term and next. We can use sum just as we would any procedure.

(define (identity x) x)

(define (inc n) (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

;; (sum-integers 1 10) ; 55

(define (sum-cubes a b)
  (sum cube a inc b))

;; (sum-cubes 1 10) ; 3025

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* (- (* 4 x) 3)
	      (- (* 4 x) 1))))
  (sum pi-term a inc b))

;; (* 8 (pi-sum 1 1000)) ; 3.141092653621043

;; The procedure sum can be used as a building block in formulating further concepts.
;; For instance, the definite integral of a function f between the limits a and b
;; can be approximated numerically using the formula:
;;
;; Int[a,b](f) = [f(a+dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2) + ...]dx
;;
;; for small values of dx. We can express this directly as a procedure:

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; (integral cube 0 1 0.01)  ; .24998750000000042
;; (integral cube 0 1 0.001) ; .249999875000001

;; (The exact value of the integral of cube between 0 and 1 is 1/4)


;; 1.3.2 Constructing Procedures Using lambda

;; Rather than define pi-next and pi-term, it would be more convenient to have a way to
;; directly specify 'the procedure that returns its input incremented by 4' and
;; 'the procedure that returns the reciprocal of its input times its input plus 2'.
;;
;; We can do this by introducing the special form 'lambda', which creates procedures.
;;
;; Then our pi-sum procedure can be expressed without defining any auxiliary procedures as

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* (- (* 4 x) 3)
			     (- (* 4 x) 1))))
       a
       (lambda (x) (+ x 1))
       b))

;; (* 8 (pi-sum 1 1000)) ; 3.141092653621043

;; Again using lambda, we can write the integral procedure without having to define
;; the auxiliary procedure add-dx:

(define (integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2.0))
	  (lambda (x) (+ x dx))
	  b)
     dx))

;; (integral cube 0 1 0.01)  ; .24998750000000042
;; (integral cube 0 1 0.001) ; .249999875000001


;; Using let to create local variables
;;
;; Another use of lambda is in creating local variables.
;; We often need local variables in our procedures other than those that have been bound
;; as formal parameters.
;;
;; For example, suppose we wish to compute the function
;;
;; f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
;;
;; which we could also express as
;;
;; a      = 1+xy
;; b      = 1-y
;; f(x,y) = xa^2 + yb + ab
;;
;; In writing a procedure to compute f, we would like to include as local variables
;; not only x and y but also the names of intermediate quantities like a and b.
;;
;; Using let, the f procedure could be written as

(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; (f 5 7) ; 6222

;; The scope of a variable specified by a 'let' expression is the body of the 'let'.
;; This implies that:
;; - 'let' allows one to bind variables as locally as possible to where they are to be used
;; - the variables' values are computed outside the 'let'. This matters when the expressions
;;   that provide the values for the local variables depend upon variables having the same names
;;   as the local variables themselves.


;; 1.3.3 Procedures as General Methods (Обобщенные методы)
;;
;; In this section, two more elaborate examples are discussed - general methods for
;; finding zeros and fixed points of functions - and it is showed how these methods
;; can be expressed directly as procedures.


;; Finding roots of equations by the half-interval method
;;
;; The half-interval method is a simple but powerful technique for finding roots of
;; an equation f(x) = 0, where f is a continuous function.
;; The idea is that, if we are given points a and b such that f(a) < 0 < f(b), then
;; f must have at least one zero between a and b.
;;
;; To locate a zero, let x be the average of a and b and compute f(x).
;; If f(x) > 0, then f must have a zero between a and x.
;; If f(x) < 0, then f must have a zero between x and b.
;; Continuing in this way, we can identify smaller and smaller intervals
;; on which f must have a zero.
;; When we reach a point where the interval is small enough, the process stops.
;;
;; Since the interval of uncertainty is reduced by half at each step of the process,
;; the number of steps required grows as θ(log(L/T)), where
;; - L is the length of the original interval
;; - T is the error tolerance (that is, the size of the interval considered 'small enough')

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

;; Search is awkward to use directly, because we can accidentally give it points at which
;; f's values do not have the required sign, in which case we get a wrong answer.
;; Instead we will use search via the following procedure, which checks to see which of
;; the endpoints has a negative function value and which has a positive value,
;; and calls the search procedure accordingly.
;; If the function has the same sign on the two given points,
;; the half-interval method cannot be used, in which case the procedure signals an error.

(define (half-interval f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (positive? a-value) (negative? b-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; The following uses the half-interval method to approximate pi as the root
;; between 2 and 4 of sin(x) = 0:
;;
;; (half-interval sin 2.0 4.0) ; 3.14111328125

;; Using the half-interval method to search for a root of the equation x^3-2x-3 = 0
;; between 1 and 2:
;;
(half-interval (lambda (x) (- (* x x x) (* 2 x) 3))
	       1.0
	       2.0) ; 1.89306640625


;; Finding fixed points of functions
;;
;; A number x is called a 'fixed point' of a function f if x satisfies the equation f(x) = x.
;;
;; For some functions f we can locate a fixed point by beginning with an initial guess
;; and applying f repeatedly,
;;
;; f(x), f(f(x)), f(f(f(x))), ...
;;
;; until the value does not change very much.
;;
;; Using this idea, we can devise a procedure 'fixed-point' that
;; takes as inputs a function and an initial guess and produces
;; an approximation to a fixed point of the function.
;;
;; We apply the function repeatedly until we find two successive values
;; whose difference is less than some prescribed tolerance:

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; For example, we can use this method to approximate the fixed point of the cosine function,
;; starting with 1 as an initial approximation:
;;
;; (fixed-point cos 1.0) ; .7390822985224023

;; Similarly, we can find a solution to the equation y = sin(y) + cos(y)
;;
(fixed-point (lambda (y) (+ (sin y) (cos y)))
	     1.0) ; 1.2587315962971173

;; The fixed-point process is reminiscent of the process we used for finding square roots
;; in section 1.1.7. Both are based on the idea of repeatedly improving a guess until
;; the result satisfies some criterion.
;; In fact, we can readily formulate the square-root computation as a fixed-point search.
;;
;; Computing the square root of some number x requires finding a y such that y^2 = x.
;; Putting this equation into the equivalent form y = x/y, we recognize that
;; we are looking for a fixed point of the function y -> x/y [maps to] and we can therefore
;; try to compute square roots as
;;
;; (define (sqrt x)
;;   (fixed-point (lambda (y) (/ x y))
;;                1.0))
;;
;; Unfortunately, this fixed-point search does not converge.
;; Consider an initial guess y1.
;; The next guess is y2 = x/y1 and the next guess is y3 = x/y2 = x/(x/y1) = y1.
;; This results in an infinite loop in which the two guesses y1 and y2 repeat over and over,
;; oscillating about the answer.
;;
;; One way to control such oscillations is to prevent the guesses from changing so much.
;; Since the answer is always between our guess y and x/y, we can make a new guess
;; that is not as far from y as x/y by averaging y with x/y, so that the next guess
;; after y is (1/2)(y + x/y) instead of x/y.
;; The process of making such a sequence of guesses is simply the process of looking for
;; a fixed point of y -> (1/2)(y + x/y)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

;; (sqrt 9)   ; 3.
;; (sqrt 100) ; 10.

;; (Note that y = (1/2)(y + x/y) is a simple transformation of the equation y = x/y;
;; to derive it, add y to both sides of the equation and divide by 2.)
;;
;; This approach of averaging successive approximations to a solution, a technique
;; that is called 'average damping', often aids the convergence of fixed-point searches.


;; 1.3.4 Procedures as Returned Values
;;
;; The ability to pass procedures as arguments significantly enhances the expressive power
;; of a programming language. Even more expressive power can be achieved by creating
;; procedures whose returned values are themselves procedures.

;; Average damping is a useful general technique.
;; Namely, given a function f, we consider the function whose value a x
;; is equal to the average of x and f(x).
;;
;; The idea of average damping can be expressed by means of the following procedure:

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; ((average-damp square) 10) ; 55 <= the average of 10 and 100

;; Using average-damp, the square-root procedure can be reformulated as follows:

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

;; (sqrt 1)   ; 1.
;; (sqrt 4)   ; 2.000000000000002
;; (sqrt 10)  ; 3.162277660168379
;; (sqrt 100) ; 10.

;; Notice how this formulation makes explicit the three ideas in the method:
;; 1) fixed-point search
;; 2) average damping
;; 3) the function y -> x/y

;; As a simple example of reuse, notice that the cube root of x is a fixed point of
;; the function y -> x/y^2, so the square-root procedure can be immediately generalized
;; to one that extracts cube roots:

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

;; (cube-root 1)    ; 1.
;; (cube-root 27)   ; 2.9999972321057697
;; (cube-root 1000) ; 10.000002544054729


;; Newton's method
;;
;; If x -> g(x) is a differentiable function, then a solution of the equation g(x) = 0
;; is a fixed point of the function x -> f(x) where
;;
;; f(x) = x - g(x)/Dg(x)
;;
;; and Dg(x) is the derivative of g evaluated at x.
;;
;; In order to implement Newton's method as a procedure, we must first express
;; the idea of derivative.
;; Derivative, like average damping, is something that transforms a function
;; into another function.
;;
;; For instance, the derivative of the function x -> x^3 is the function x -> 3x^2.
;;
;; In general, if g is a function and dx is a small number, then the derivative Dg of g
;; is the function whose values at any number x is given (in the limit of small dx) by
;;
;; Dg(x) = (g(x+dx) - g(x)) / dx
;;
;; Thus, the idea of derivative can be expressed as the procedure (taking dx to be 0.00001):

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

;; Like average-dump, deriv is a procedure that takes a procedure as argument
;; and returns a procedure as value.
;; For example, to approximate the derivative of x -> x^3 at 5
;; (whose exact value is 75) we can evaluate

(define (cube x) (* x x x))

;; ((deriv cube) 5) ; 75.00014999664018

;; With the aid of deriv, we can express Newton's method as a fixed-point process:

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
	    ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; For instance, to find a square root of x, we can use Newton's method
;; to find a zero of the function
;;
;; y -> y^2 - x
;;
;; starting with an initiall guess of 1.
;;
;; This provides yet another form of the square-root procedure:

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

;; (sqrt 1)   ; 1.
;; (sqrt 4)   ; 2.0000000000002385
;; (sqrt 9)   ; 3.000000000000002
;; (sqrt 10)  ; 3.162277660168388
;; (sqrt 100) ; 10.


;; Abstractions and first-class procedures
;;
;; Each of the Newton's methods described above begins with a function and finds
;; a fixed point of some transformation of the function.
;; We can express this general idea itself as a procedure:

(define (fixed-point-of-transformation g transform guess)
  (fixed-point (transform g) guess))

;; This very general procedure takes as its arguments a procedure g that computes some function,
;; a procedure that transforms g, and an initial guess.
;; The returned result is a fixed point of the transformed function.
;;
;; Using this abstraction, the first square-root computation from this section can be recast
;; as an instance of this general method:

(define (sqrt x)
  (fixed-point-of-transformation (lambda (y) (/ x y))
				 average-damp
				 1.0))

;; (sqrt 1)   ; 1.
;; (sqrt 4)   ; 2.000000000000002
;; (sqrt 9)   ; 3.
;; (sqrt 10)  ; 3.162277660168379
;; (sqrt 100) ; 10.

;; Similarly, we can express the second square-root computation from this section (an instance of
;; Newton's method that finds a fixed point of the Newton transform y -> y^2 - x) as:

(define (sqrt x)
  (fixed-point-of-transformation (lambda (y) (- (square y) x))
				 newton-transform
				 1.0))

;; (sqrt 1)   ; 1.
;; (sqrt 4)   ; 2.0000000000002385
;; (sqrt 9)   ; 3.000000000000002
;; (sqrt 10)  ; 3.162277660168388
;; (sqrt 100) ; 10.

;; We began section 1.3 with the observation that compound procedures are a crucial
;; abstraction mechanism, because they permit us to express general methods of computing
;; as explicit elements in our programming language.
;;
;; Now we've seen how higher-order procedures permit us to manipulate these general methods
;; to create further abstractions.
