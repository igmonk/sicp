;; Compile begin
;;
;; Extends the given compiler with begin expressions.

(define (install-compile-begin compiler)
  (let ((compile (compiler 'compile))
        (compile-sequence (compiler 'compile-sequence))
        (extend-compile (compiler 'extend-compile))
        (def-constructor (compiler 'def-constructor)))

    (define (compile-begin exp target linkage cenv)
      (compile-sequence (begin-actions exp) target linkage cenv))

    (define (make-begin seq) (cons 'begin seq))
    (define (begin-actions exp) (cdr exp))

    (extend-compile 'begin compile-begin)
    (def-constructor 'make-begin make-begin)))
