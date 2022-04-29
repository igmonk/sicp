;; Compiler Factory

(load "compiler.scm")

(load "compile-assignment.scm")
(load "compile-definition.scm")
(load "compile-quoted.scm")
(load "compile-if.scm")
(load "compile-lambda.scm")
(load "compile-begin.scm")
(load "compile-let.scm")

(define (create-compiler)
  (let ((compiler (make-compiler)))
    (install-compile-assignment compiler)
    (install-compile-definition compiler)
    (install-compile-quoted compiler)
    (install-compile-if compiler)
    (install-compile-lambda compiler)
    (install-compile-begin compiler)
    (install-compile-let compiler)
    compiler))
