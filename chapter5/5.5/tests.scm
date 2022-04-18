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


;; Compile and go
(load "compile-and-go.scm")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;; (total-pushes = 0 maximum-depth = 0)

;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5) ;; (total-pushes = 31 maximum-depth = 14)

;;; EC-Eval value:
120

;; The interpreted version required 144 pushes and
;; a maximum stack depth of 28!
;;
;; This illustrates the optimization that results from
;; the compilation strategy.
