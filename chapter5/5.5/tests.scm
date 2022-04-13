;; Compiler Tests

(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-factorial
  (compile
   '(define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
   'val
   'next))

;; Save to a file if needed.
(instruction-seq->file compiled-factorial "factorial.obj")
