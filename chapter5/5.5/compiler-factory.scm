;; Compiler Factory

(load "compiler.scm")

(load "compile-assignment.scm")
(load "compile-definition.scm")
(load "compile-quoted.scm")
(load "compile-if.scm")
(load "compile-lambda.scm")

(define (create-compiler)
  (let ((compiler (make-compiler)))
    (install-compile-assignment compiler)
    (install-compile-definition compiler)
    (install-compile-quoted compiler)
    (install-compile-if compiler)
    (install-compile-lambda compiler)
    compiler))
