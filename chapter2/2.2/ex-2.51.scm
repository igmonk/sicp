;; Exercise 2.51
;;
;; Define the 'below' operation for painters.
;;
;; 'below' takes two painters as arguments. The resulting painter,
;; given a frame, draws with the first painter in the bottom of the frame
;; and with the second painter in the top.
;;
;; Define 'below' in two different ways:
;; 1) by writing a procedure that is analogous to the 'beside' procedure, and
;; 2) in terms of 'beside' and suitable rotation operations (from exercise 2.50)

(load "workbook.scm")
(load "ex-2.46.scm")
(load "ex-2.50.scm")


;; 1) 'below' that is analogous to the 'beside' procedure

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((bottom-painter (transform-painter painter1
                                             (make-vect 0.0 0.0)
                                             (make-vect 1.0 0.0)
                                             split-point))
          (top-painter (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.5)
                                          (make-vect 0.0 1.0))))
      (lambda (frame)
        (bottom-painter frame)
        (top-painter frame)))))


;; 2) 'below' in terms of 'beside' and suitable rotation operations
;;
;; The following transformation is applied:
;;
;; below(painter1, painter2) = rotate90(beside(rotate270(painter1), rotate270(painter2)))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
