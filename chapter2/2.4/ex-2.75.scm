;; Exercise 2.75
;;
;; Implement the constructor 'make-from-mag-ang' in message-passing style.
;; This procedure should be analogous to the 'make-from-real-imag' procedure
;; given above.

(load "workbook.scm")

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


;; (define z (make-from-mag-ang 10 20))
;;
;; (real-part z) ; 4.080820618133919
;; (imag-part z) ; 9.129452507276277
;; (magnitude z) ; 10
;; (angle z)     ; 20
