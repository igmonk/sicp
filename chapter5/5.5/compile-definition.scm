;; Compile Definition

(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (install-compile-definition compiler)
  (let ((compile (compiler 'compile))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage))
        (def-constructor (compiler 'def-constructor))
        (get-constructor (compiler 'get-constructor))
        (extend-syntax (compiler 'extend-syntax)))

    (define (compile-definition exp target linkage cenv)
      (let ((var (definition-variable exp))
            (get-value-code
             (compile (definition-value exp) 'val 'next cenv)))
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

    (define (make-define var value)
      (cons 'define (cons var value)))

    ;; Dependency constructors
    (define (make-lambda . args)
      (apply (get-constructor 'make-lambda) args))

    (extend-compile 'define compile-definition)
    (extend-syntax 'definition-variable definition-variable)
    (extend-syntax 'definition-value definition-value)
    (def-constructor 'make-define make-define)))
