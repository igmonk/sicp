;; Compile Lambda
;;
;; The object code for a 'lambda' expression must have the form
;;
;; <construct procedure object and assign it to target register>
;; <linkage>
;;
;; When the 'lambda' expression is compiled, the code for
;; the procedure body is also generated.
;;
;; Although the body won't be executed at the time of procedure
;; construction, it is convenient to insert it into the object
;; code right after the code for the lambda.
;;
;; The object code thus has the form
;;
;;  <construct procedure object and assign it to target register>
;;  <code for given linkage> or (goto (label after-lambda))
;;  <compilation of procedure body>
;; after-lambda

(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (install-compile-lambda compiler)
  (let ((compile (compiler 'compile))
        (compile-sequence (compiler 'compile-sequence))
        (make-label (compiler 'make-label))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage)))

    (define (compile-lambda exp target linkage)
      (let ((proc-entry (make-label 'entry))
            (after-lambda (make-label 'after-lambda)))
        (let ((lambda-linkage
               (if (eq? linkage 'next) after-lambda linkage)))
          (append-instruction-sequences
           (tack-on-instruction-sequence
            (end-with-linkage
             lambda-linkage
             (make-instruction-sequence
              '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
            (compile-lambda-body exp proc-entry))
           after-lambda))))

    (define (compile-lambda-body exp proc-entry)
      (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
         (make-instruction-sequence
          '(env proc argl) '(env)
          `(,proc-entry
            (assign env (op compiled-procedure-env) (reg proc))
            (assign env
                    (op extend-environment)
                    (const ,formals)
                    (reg argl)
                    (reg env))))
         (compile-sequence (lambda-body exp) 'val 'return))))

    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    (extend-compile 'lambda compile-lambda)))
