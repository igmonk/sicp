;; Compile Definition

(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (install-compile-definition compiler)
  (let ((compile (compiler 'compile))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage)))

    (define (compile-definition exp target linkage)
      (let ((var (definition-variable exp))
            (get-value-code
             (compile (definition-value exp) 'val 'next)))
        (end-with-linkage
         linkage
         (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op define-variable!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))))))

    (define (definition-variable exp)
      (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))

    (define (definition-value exp)
      (if (symbol? (cadr exp))
          (caddr exp)
          (make-lambda (cdadr exp)   ; formal parameters
                       (cddr exp)))) ; body

    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))

    (extend-compile 'define compile-definition)))
