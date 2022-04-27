;; Lexical Address machine operations

(load "../5.4/frame.scm")
(load "../5.4/frame-binding.scm")
(load "lexical-address.scm")

(define (lexical-addr-lookup lexical-addr env)
  (let* ((binding (lexical-addr-binding-lookup lexical-addr env))
         (value (frame-binding-value binding)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable -- LEXICAL-ADDR-LOOKUP" lexical-addr)
        value)))

(define (lexical-addr-set! lexical-addr val env)
  (let ((binding (lexical-addr-binding-lookup lexical-addr env)))
    (if (not binding)
        (error "Unbound variable -- LEXICAL-ADDR-SET!" lexical-addr)
        (begin
          (frame-binding-set-value! val binding)
          'ok))))

(define (lexical-addr-binding-lookup lexical-addr env)
  (let ((frame-num (lexical-addr-frame-num lexical-addr)))
    (bindings-ref (lexical-addr-displacement lexical-addr)
                  ((env-ref env frame-num) 'get-frame))))

(define (env-ref env index)
  (cond ((<= index 0) env)
        ((env 'empty?) (error "Empty environment -- ENV-REF"))
        (else (env-ref (env 'get-base-env) (- index 1)))))
