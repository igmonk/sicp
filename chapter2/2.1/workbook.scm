;; 2.1 Introduction to Data Abstraction

;; Example: Arithmetic Operations for Rational Numbers
;;
;; Suppose we want to do arithmetic with rational numbers.
;; We want to be able to add, subtract, multiply, and divide them and
;; to test whether two rational numbers are equal.
;;
;; Let us begin by assuming that we already have a way of constructing a rational number
;; from a numerator and a denominator. We also assume that, given a rational number,
;; we have a way of extracting (or selecting) its numerator and its denominator.
;;
;; Let us further assume that the constructor and selectors are available as procedures:
;; - (make-rat <n> <d>) -> returns the rational number
;; - (numer <x>)        -> returns the numerator
;; - (denom <x>)        -> returns the denominator
;;
;; Here we are using a powerful strategy of synthesis: wishful thinking.
;; We haven't yet said how a rational number is represented,
;; or how the procedures numer, denom, and make-rat should be implemented.
;;
;; The rules to add, subtract, multiply, divide, and test equality can be expressed as procedures:

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Now we have the operations on rational numbers defined in terms of
;; the selector and constructor procedures numer, denom, and make-rat.
;;
;; To enable us to implement the concrete level of our data abstraction,
;; our language provides a compound structure called a pair,
;; which can be constructed with the primitive procedure 'cons'.

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;; In order to display the results of our computations, we can print rational numbers
;; by printing the numerator, a slash, and the denominator:

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Now we can try our rational-number procedures:

(define one-half (make-rat 1 2))

;; (print-rat one-half) ; 1/2

(define one-third (make-rat 1 3))

;; (print-rat (add-rat one-half one-third))  ; 5/6
;; (print-rat (mul-rat one-half one-third))  ; 1/6
;; (print-rat (add-rat one-third one-third)) ; 6/9

;; As the final example shows, our rational-number implementation
;; does not reduce rational numbers to lowest terms.
;; We can remedy this by changing make-rat.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; (print-rat (add-rat one-third one-third)) ; 2/3


;; What is meant by data?
;;
;; It is not enough to say 'whatever is implemented by the given selectors and constructors.'
;; Clearly, not every arbitrary set of three procedures can serve as
;; an appropriate basis for the rational-number implementation.
;;
;; We need to guarantee that, if we construct a rational number x
;; from a pair of integers n and d, then extracting the numer and the denom of x
;; and dividing them should yield the same result as dividing n by d.
;;
;; In other words, make-rat, numer, and denom must satisfy the condition that,
;; for any integer n and any non-zero integer d, if x is (make-rat n d), then
;;
;; (numer x) / (denom x) = n / d
;;
;; In fact, this is the only condition make-rat, numer, and denom must fulfill
;; in order to form a suitable basis for a rational-number representation.
;;
;; In general, we can think of data as defined by some collection
;; of selectors and constructors, together with specified conditions that
;; these procedures must fulfill in order to be a valid representation.
;;
;; Consider the notion of a pair.
;; It was never said what a pair was, only that the language supplied procedures
;; 'cons', 'car', and 'cdr' for operating on pairs.
;; But the only thing we need to know about these three operations is that
;; if we glue two objects together using 'cons' we can retrieve the objects
;; using 'car' and 'cdr'.
;; That is, the operations satisfy the condition that, for any objects x and y,
;; if z is (cons x y) then (car z) is x and (cdr z) is y.
;;
;; Indeed, these three procedures are included as primitives in the language.
;; However, any triple of procedures that satisfies the above condition can be used
;; as the basis for implementing pairs.
;; This point is illustrated strikingly by the fact that we could implement
;; cons, car, and cdr without using any data structures at all but only using procedures.
;;
;; Here are the definitions:

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z)
  (z 0))

(define (cdr z)
  (z 1))

;; (car (cons 1 2)) ; 1
;; (cdr (cons 1 2)) ; 2

;; As long as only 'cons', 'car', and 'cdr' are used to access pairs,
;; this procedural implementation is indistinguishable from one that uses
;; 'real' data structures.


;; Extended Exercise: Interval Arithmetic

;; An engineering problem: the ability to manipulate inexact quantities
;; (such as measured parameters of physical devices) with known precision,
;; so that when computations are done with such approximate quantities
;; the results will be numbers of known precision.

;; It is sometimes necessary for electrical engineers to compute the value of
;; a parallel equivalent resistance Rp of two resistors R1 and R2
;; using the formula
;;
;; Rp = 1 / (1/R1 + 1/R2)
;;
;; Resistance values are usually known only up to some tolerance
;; guaranteed by the manufacturer of the resistor.
;;
;; For example, if a resistor is labeled '6.8 ohms with 10% tolerance',
;; one can only be sure that the resistor has a resistance between
;; 6.8 - 0.68 = 6.12 and 6.8 + 0.68 = 7.48 ohms.
;;
;; Thus, if you have a 6.8-ohm 10% resistor in parallel with a 4.7-ohm 5% resistor,
;; the resistance of the combination can range
;; from about 2.58 ohms (if the two resistors are at the lower bounds)
;; to about 2.97 ohms (if the two resistors are at the upper bounds).
;;
;; The idea is to implement 'interval arithmetic' as a set of arithmetic operations
;; for combining 'intervals' (objects that represent the range of possible values of
;; an inexact quantity). The result of adding, subtracting, multiplying, or dividing
;; two intervals is itself an interval, representing the range of the result.

;; It is possible to postulate the existence of an abstract object called an 'interval'
;; that has two endpoints: a lower bound and an upper bound.
;; Given the endpoints of an interval, one can construct the interval using
;; the data constructor 'make-interval'.

;; A procedure for adding two intervals.
;; The minimum value the sum could be is the sum of the two lower bounds
;; and the maximum value it could be is the sum of the two upper bounds:

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; The product of two intervals.
;; The lower-bound and upper-bound values are the minimum and the maximum of
;; the products of the bounds.

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; The devision of two intervals.
;; To divide two intervals, multiply the first by the reciprocal of the second.
;; Note that the bounds of the reciprocal interval are the reciprocal of
;; the upper bound and the reciprocal of the lower bound, in that order.

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
