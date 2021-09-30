;; Use segments->painter to define the following primitive painters:
;;
;; a. The painter that draws the outline of the designated frame.
;; b. The painter that draws an 'X' by connecting opposite corners of the frame.
;; c. The painter that draws a diamond shape by connecting the midpoints
;;    of the sides of the frame.
;; d. The 'wave' painter.


;; a. The painter that draws the outline of the designated frame.

(define outline-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0)
                       (make-vect 0.0 1.0))
         (make-segment (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 1.0)
                       (make-vect 1.0 0.0))
         (make-segment (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)))))


;; b. The painter that draws an 'X' by connecting opposite corners of the frame.

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
         (make-segment (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))))


;; c. The painter that draws a diamond shape by connecting the midpoints
;;    of the sides of the frame.

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5)
                       (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0)
                       (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5)
                       (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0)
                       (make-vect 0.0 0.5)))))


;; d. The 'wave' painter.

;; srsly ?)
