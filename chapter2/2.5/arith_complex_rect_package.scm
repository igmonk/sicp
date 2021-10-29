;; Complex numbers - Rectangular representation

(load "../export-defs.scm")
(load "./arith_lib.scm")

(define (install-complex-rect-package)
  ;; utils
  (define (add x y) (apply-generic 'add x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (sine x) (apply-generic 'sine x))
  (define (cosine x) (apply-generic 'cosine x))
  (define (square x) (apply-generic 'square x))
  (define (sqrt x) (apply-generic 'sqrt x))
  (define (atan2 x y) (apply-generic 'atan2 x y))

  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atan2 (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a))
          (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (list (export-def 'real-part '(rectangular) real-part)
        (export-def 'imag-part '(rectangular) imag-part)
        (export-def 'magnitude '(rectangular) magnitude)
        (export-def 'angle '(rectangular) angle)
        (export-def 'make-from-real-imag 'rectangular
                    (lambda (x y) (tag (make-from-real-imag x y))))
        (export-def 'make-from-mag-ang 'rectangular
                    (lambda (r a) (tag (make-from-mag-ang r a))))))
