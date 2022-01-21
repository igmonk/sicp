;; Real numbers package
;;
;; The procedures raise and project will fail if the necessary
;; complex and rational numbers packages are not installed.

(load "../export-defs.scm")

(define (install-real-package)
  ;; internal procedures
  (define (make-rational n d)
    ((get 'make 'rational) n d))
  (define (make-complex-from-real-imag r i)
    ((get 'make-from-real-imag 'complex) r i))
  (define (raise x)
    (make-complex-from-real-imag x 0))
  (define (project x)
    (let ((scheme-rat
           (rationalize
            (inexact->exact x) 1/100)))
      (make-rational (numerator scheme-rat)
                     (denominator scheme-rat))))

  ;; interface to rest of the system
  (define types-arity-5
    '(real real real real real))
  (list (export-def 'add '(real real)
                    (lambda (x y) (+ x y)))
        (export-def 'sub '(real real)
                    (lambda (x y) (- x y)))
        (export-def 'mul '(real real)
                    (lambda (x y) (* x y)))
        (export-def 'div '(real real)
                    (lambda (x y) (/ x y)))
        (export-def 'add5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (+ n1 n2 n3 n4 n5)))
        (export-def 'mul5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (* n1 n2 n3 n4 n5)))
        (export-def 'equ? '(real real)
                    (lambda (x y) (= x y)))
        (export-def 'zero? '(real)
                    (lambda (x) (= x 0)))
        (export-def 'neg '(real)
                    (lambda (x) (- x)))
        (export-def 'sine '(real) sin)
        (export-def 'cosine '(real) cos)
        (export-def 'square '(real) square)
        (export-def 'sqrt '(real) sqrt)
        (export-def 'atan2 '(real real) atan)
        (export-def 'raise '(real) raise)
        (export-def 'project '(real) project)
        (export-def 'make 'real
                    (lambda (x) x))))

;; Notice, the procedure 'rationalize' is used when projecting
;; real numbers to rationals:
;;
;; > rationalize returns the simplest rational number
;;   differing from x by no more than y
;;
;; See: https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_5.html
