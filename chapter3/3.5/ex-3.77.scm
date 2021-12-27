;; Exercise 3.77
;;
;; The integral procedure used above was analogous to the "implicit" definition
;; of the infinite stream of integers in section 3.5.2.
;;
;; Alternatively, we can give a definition of integral that is more like
;; integers-starting-from (also in section 3.5.2):

(load "workbook.scm")

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* (stream-car integrand) dt)
                                initial-value)
                             dt))))

;; When used in systems with loops, this procedure has the same problem
;; as does our original version of integral.
;;
;; Modify the procedure so that it expects the integrand
;; as a delayed argument and hence can be used in the solve procedure
;; shown above.

(define (integral-delayed delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral-delayed (delay (stream-cdr integrand))
                                       (+ (* (stream-car integrand) dt)
                                          initial-value)
                                       dt)))))

;; Approximating e ~ 2.718
;;
;; (stream-ref (solve (lambda (y) y) 1 0.001) 1000) ; 2.716923932235896
