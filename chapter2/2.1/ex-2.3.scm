;; Exercise 2.3
;;
;; Implement a representation for rectangles in a plane.
;; (Hint: You may want to make use of exercise 2.2.)
;;
;; In terms of your constructors and selectors, create procedures that compute
;; the perimeter and the area of a given rectangle.
;;
;; Now implement a different representation for rectangles.
;;
;; Can you design your system with suitable abstraction barriers, so that
;; the same perimeter and area procedures will work using either representation?

(load "ex-2.3.scm")


;; 1. Create a rectangle representation that is based on a line segment
;;    representing the rectangle's diagonal.

(define (make-rect x y w h)
  (make-segment (make-point x y)
                (make-point (+ x w) (+ y h))))

(define (width r)
  (abs (- (x-point (start-segment r))
          (x-point (end-segment r)))))

(define (height r)
  (abs (- (y-point (start-segment r))
          (y-point (end-segment r)))))


;; 2. Create a rectangle representation that is based on the given point, width and height.

(define (make-rect x y w h)
  (cons (make-point x y)
        (cons w h)))

(define (width r)
  (car (cdr r)))

(define (height r)
  (cdr (cdr r)))


;; As long as the representation for rectangles provides the width and length selectors,
;; the perimeter and area procedures, which belong to a higher abstraction level, will stay unaltered.

(define (rect-perimeter r)
  (* 2 (+ (width r) (height r))))

(define (rect-area r)
  (* (width r) (height r)))


;; Rectangles

(define rect1 (make-rect 1 1 10 20))

;; (rect-perimeter rect1) ; 60
;; (rect-area rect1)      ; 200

(define rect2 (make-rect -5 -10 10 5))

;; (rect-perimeter rect2) ; 30
;; (rect-area rect2)      ; 50


;; Data-abstraction barriers:
;;
;;
;;            rect-perimeter / rect-area
;; ------------------------------------------------
;;            make-rect / width / height
;; ------------------------------------------------
;;    make-segment / start-segment / end-segment      (omitted in the 2nd representation)
;; ------------------------------------------------
;;          make-point / x-point / y-point
;; ------------------------------------------------
;;                 cons / car / cdr
