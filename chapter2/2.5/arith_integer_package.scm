;; Integer numbers package
;;
;; The procedure raise will fail if the necessary rational numbers
;; package is not installed.

(load "../export-defs.scm")

(define (install-integer-package)
  ;; internal procedures
  (define (raise x)
    ((get 'make 'rational) x 1))

  ;; interface to rest of the system
  (define types-arity-5
    '(integer integer integer integer integer))
  (list (export-def 'add '(integer integer)
                    (lambda (x y) (+ x y)))
        (export-def 'sub '(integer integer)
                    (lambda (x y) (- x y)))
        (export-def 'mul '(integer integer)
                    (lambda (x y) (* x y)))
        (export-def 'div '(integer integer)
                    (lambda (x y) (/ x y)))
        (export-def 'add5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (+ n1 n2 n3 n4 n5)))
        (export-def 'mul5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (* n1 n2 n3 n4 n5)))
        (export-def 'equ? '(integer integer)
                    (lambda (x y) (= x y)))
        (export-def '=zero? '(integer integer)
                    (lambda (x) (= x 0)))
        (export-def 'neg '(integer)
                    (lambda (x) (- x)))
        (export-def 'sine '(integer) sin)
        (export-def 'cosine '(integer) cos)
        (export-def 'square '(integer) square)
        (export-def 'sqrt '(integer) sqrt)
        (export-def 'atan2 '(integer integer) atan)
        (export-def 'raise '(integer) raise)
        (export-def 'make 'integer
                    (lambda (x) x))))
