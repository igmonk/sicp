;; Eval Lambda
;;
;; Extends the given evaluator with lambda expressions.

(load "list-utils.scm")
(load "procedure.scm")

(define (install-eval-lambda evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor))
        (get-constructor (evaluator 'get-constructor))
        (analyze-sequence (evaluator '_analyze-seq))
        (get-syntax (evaluator 'get-syntax)))

    (define (eval-lambda exp env)
      (make-procedure (lambda-parameters exp)
                      (scan-out-defines (lambda-body exp))
                      env))

    (define (analyze-lambda exp)
      (let ((vars (lambda-parameters exp))
            (bproc (analyze-sequence
                    (scan-out-defines (lambda-body exp)))))
        (lambda (env) (make-procedure vars bproc env))))
    
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))
    
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    ;; Scans the procedure body with internal definitions
    ;;
    ;; (lambda <vars>
    ;;   (define u <e1>)
    ;;   (define v <e2>)
    ;;   <e3>)
    ;;
    ;; and returns a let-equivalent without internal definitions
    ;;
    ;; (lambda <vars>
    ;;   (let ((u '*unassigned*)
    ;;         (v '*unassigned*))
    ;;     (set! u <e1>)
    ;;     (set! v <e2>)
    ;;     <e3>))
    (define (scan-out-defines proc-body)
      (let ((defines (filter define? proc-body)))
        (if (null? defines)
            proc-body
            (let ((other-exps (remove define? proc-body))
                  (def-vars (map (get-syntax 'definition-variable) defines))
                  (def-vals (map (get-syntax 'definition-value) defines)))
              (list ; list since it still is a procedure body (sequence)
               (make-let
                (map var->unassigned-pair def-vars)
                (append (map var-val->make-set! def-vars def-vals)
                        other-exps)))))))

    (define (define? exp) (tagged-list? exp 'define))

    (define (var->unassigned-pair var)
      (list var (quote '*unassigned*)))

    (define (var-val->make-set! var val)
      (make-set! var (list val)))

    ;; Dependency constructors
    (define (make-let . args)
      (apply (get-constructor 'make-let) args))
    (define (make-set! . args)
      (apply (get-constructor 'make-set!) args))
    
    (extend-eval 'lambda eval-lambda)
    (extend-analyze 'lambda analyze-lambda)
    (def-constructor 'make-lambda make-lambda)))
