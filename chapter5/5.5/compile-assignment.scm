;; Compile Assignment

(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (install-compile-assignment compiler)
  (let ((compile (compiler 'compile))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage)))

    (define (compile-assignment exp target linkage)
      (let ((var (assignment-variable exp))
            (get-value-code
             (compile (assignment-value) 'val 'next)))
        (end-with-linkage
         linkage
         (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))))))

    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))

    (extend-compile 'set! compile-assignment)))
