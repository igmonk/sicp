;; Compile Assignment

(load "instruction-seq.scm")
(load "instruction-comb.scm")
(load "cenvironment.scm")

(define (install-compile-assignment compiler)
  (let ((compile (compiler 'compile))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage))
        (def-constructor (compiler 'def-constructor)))

    (define (compile-assignment exp target linkage cenv)
      (let ((var (assignment-variable exp))
            (get-value-code
             (compile (assignment-value exp) 'val 'next cenv)))
        (let ((lexaddr (find-variable exp cenv)))
          (end-with-linkage
           linkage
           (preserving
            '(env)
            get-value-code
            (make-instruction-sequence
             '(env val) (list target)
             (if (eq? 'not-found lexaddr)
                 `((perform (op set-variable-value!)
                            (const ,var)
                            (reg val)
                            (reg env))
                   (assign ,target (const ok)))
                 `((perform (op lexical-addr-set!)
                            (const ,lexaddr)
                            (reg val)
                            (reg env))
                   (assign ,target (const ok))))))))))

    (define (make-set! var value)
      (cons 'set! (cons var value)))

    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))

    (extend-compile 'set! compile-assignment)
    (def-constructor 'make-set! make-set!)))
