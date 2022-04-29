;; Compile let
;;
;; Extends the given compiler with let expressions.

(load "list-utils.scm")

(define (install-compile-let compiler)
  (let ((compile (compiler 'compile))
        (extend-compile (compiler 'extend-compile))
        (def-constructor (compiler 'def-constructor))
        (get-constructor (compiler 'get-constructor)))

    (define (compile-let exp target linkage cenv)
      (compile (let->combination exp) target linkage cenv))

    (define (let->combination exp)
      (if (symbol? (cadr exp)) ; if named let
          (named-let->combination exp)
          (ordinary-let->combination exp)))

    (define (named-let->combination exp)
      (let ((name (cadr exp))
            (var-exp-pairs (caddr exp))
            (body (cdddr exp)))
        (let ((params (car (decouple-pairs var-exp-pairs))))
          (make-begin
           (list (make-define (cons name params) body)
                 (make-let var-exp-pairs body))))))
    
    (define (ordinary-let->combination exp)
      (let ((var-exps (decouple-pairs (let-var-exp-pairs exp)))
            (body (let-body exp)))
        (append (list (make-lambda (car var-exps) body))
                (cadr var-exps))))

    (define (make-let var-exp-pairs body)
      (cons 'let (cons var-exp-pairs body)))
    (define (let-var-exp-pairs exp) (cadr exp))
    (define (let-body exp) (cddr exp))

    (define (make-named-let name var-exp-pairs body)
      (cons 'let (cons name (cons var-exp-pairs body))))

    ;; Dependency constructors
    (define (make-begin . args)
      (apply (get-constructor 'make-begin) args))
    (define (make-define . args)
      (apply (get-constructor 'make-define) args))
    (define (make-lambda . args)
      (apply (get-constructor 'make-lambda) args))

    (extend-compile 'let compile-let)
    (def-constructor 'make-let make-let)))
