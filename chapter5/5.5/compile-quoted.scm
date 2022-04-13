;; Compile Quoted

(load "instruction-seq.scm")

(define (install-compile-quoted compiler)
  (let ((extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage)))

    (define (compile-quoted exp target linkage)
      (end-with-linkage
       linkage
       (make-instruction-sequence
        '() (list target)
        `((assign ,target (const ,(text-of-quotation exp)))))))

    (define (text-of-quotation exp) (cadr exp))

    (extend-compile 'quote compile-quoted)))
