;; Exercise 2.50
;;
;; Define the transformation flip-horiz, which flips painters horizontally, and
;; transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

(load "workbook.scm")
(load "ex-2.46.scm")


;; Note: the details of how primitive painters are implemented depend on
;;       the particular characteristics of the graphics system and
;;       the type of image to be drawn.
;;       Here, a screen is assumed to have its origin at (0,0), and
;;       each frame's origin is its bottom-left corner.


;; Flip painters horizontally

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;; Rotate painters by 180 degrees

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))


;; Rotate painters counterclockwise by 270 degrees

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
