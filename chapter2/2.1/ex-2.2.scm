;; Exercise 2.2
;;
;; Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point and an ending point.
;;
;; Define a constructor 'make-segment' and selectors 'start-segment' and 'end-segment'
;; that define the representation of segments in terms of points.

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;; Furthermore, a point can be represented as pair of numbers:
;; the x coordinate and the y coordinate.
;;
;; Accordingly, specify a constructor 'make-point' and selectors 'x-point' and 'y-point'
;; that define this representation.

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;; Finally, using your selectors and constructors, define a procedure 'midpoint-segment'
;; that takes a line segment as argument and returns its midpoint
;; (the point whose coordinates are the average of the coordinates of the endpoints).

(define (midpoint-segment s)
  (let ((start-point (start-segment s))
        (end-point (end-segment s)))
    (make-point (avg2 (x-point start-point) (x-point end-point))
                (avg2 (y-point start-point) (y-point end-point)))))

(define (avg2 a b)
  (/ (+ a b) 2))

;; To try your procedures, you'll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define p1 (make-point 1 2))
(define p2 (make-point 10 12))

;; (print-point p1) ; (1,2)
;; (print-point p2) ; (10,12)

(define s1 (make-segment p1 p2))

(define m1 (midpoint-segment s1))

;; (print-point m1) ; (11/2,7)
