;; Complex numbers - Polar representation

(load "../export-defs.scm")
(load "./arith_lib.scm")

(define (install-complex-polar-package)
  ;; utils
  (define (add x y) (apply-generic 'add x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (sine x) (apply-generic 'sine x))
  (define (cosine x) (apply-generic 'cosine x))
  (define (square x) (apply-generic 'square x))
  (define (sqrt x) (apply-generic 'sqrt x))
  (define (atan2 x y) (apply-generic 'atan x y))

  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))
          (atan2 y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (list (export-def 'magnitude '(polar) magnitude)
        (export-def 'angle '(polar) angle)
        (export-def 'real-part '(polar) real-part)
        (export-def 'imag-part '(polar) imag-part)
        (export-def 'make-from-mag-ang 'polar
                    (lambda (x y) (tag (make-from-mag-ang x y))))
        (export-def 'make-from-real-imag 'polar
                    (lambda (x y) (tag (make-from-real-imag x y))))))
