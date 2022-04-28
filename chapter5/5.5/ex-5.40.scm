;; Exercise 5.40
;;
;; Modify the compiler to maintain the compile-time environment
;; as described above. That is, add a compile-time-environment
;; argument to compile and the various code generators, and extend
;; it in compile-lambda-body.


;; New compile-lambda-body (see compile-lambda.scm):

(define (compile-lambda-body exp proc-entry cenv)
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
     (compile-sequence (lambda-body exp)
                       'val
                       'return
                       (extend-cenvironment formals cenv)))))


;; When a lambda body is compiled, compile-lambda-body extends
;; the compile-time environment by a frame containing
;; the procedure's parameters, so that the sequence making up
;; the body is compiled with that extended environment.
