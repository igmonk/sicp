;; Evaluator
;;
;; The evaluator whose dispatch is done in data-directed style.
;;
;; The evaluation process is be described as the interplay between
;; two procedures: 'eval' and 'apply'.
;;
;; In order not to mess up with the built-in eval and apply procedures,
;; their counterparts are to be prefixed with the underscore symbol.

(load "list-utils.scm")
(load "environment.scm")
(load "procedure.scm")
(load "thunk.scm")

(define (make-evaluator edt)
  (let ((get (edt 'lookup-proc))
        (put (edt 'insert-proc!)))

    ;; Eval with preceeding syntactic analysis
    ;; (define (_eval exp env)
    ;;   ((_analyze exp) env))

    ;; Eval (normal-order) mixed with syntactic analysis
    (define (_eval exp env)
      (cond ((self-evaluating? exp) exp)
            ((variable? exp) (lookup-variable-value exp env))
            ((classify exp '_eval) ((classify exp '_eval) exp env))
            ((application? exp)
             (_apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
            (else
             (error "Unknown expression type -- EVAL" exp))))

    ;; Normal-order:
    ;; - for primitive procedures (which are strict),
    ;;   evaluate all the arguments before applying the primitive
    ;; - for compound procedures (which are non-strict)
    ;;   delay all the arguments before applying the procedure
    (define (_apply procedure arguments env)
      (cond ((primitive-procedure? procedure)
             (apply-primitive-procedure
              procedure
              (list-of-arg-values arguments env))) ; changed
            ((compound-procedure? procedure)
             (eval-sequence
              (procedure-body procedure)
              (extend-environment
               (param-names (procedure-parameters procedure))
               ;; (list-of-args (procedure-parameters procedure) ; upward-compatible
               ;;               arguments
               ;;               env)
               (list-of-delayed-args arguments env) ; changed
               (procedure-environment procedure))))
            (else
             (error "Unknown procedure type -- APPLY" procedure))))

    (define (_analyze exp)
      (cond ((self-evaluating? exp)
             (analyze-self-evaluating exp))
            ((variable? exp) (analyze-variable exp))
            ((classify exp '_analyze)
             ((classify exp '_analyze) exp))
            ((application? exp) (analyze-application exp))
            (else
             (error "Unknown expression type -- ANALYZE" exp))))

    (define (analyze-application exp)
      (let ((fproc (_analyze (operator exp)))
            (aprocs (map _analyze (operands exp))))
        (lambda (env)
          (execute-application (fproc env)
                               (map (lambda (aproc) (aproc env))
                                    aprocs)))))

    (define (execute-application proc args)
      (cond ((primitive-procedure? proc)
             (apply-primitive-procedure proc args))
            ((compound-procedure? proc)
             ((procedure-body proc)
              (extend-environment
               (procedure-parameters proc)
               args
               (procedure-environment proc))))
            (else
             (error "Unknown procedure type -- EXECUTE-APPLICATION"
                    proc))))

    (define (list-of-args params exps env)
      (if (or (null? params) (no-operands? exps))
          '()
          (cons (arg-exp->arg (car params) (first-operand exps) env)
                (list-of-args (cdr params) (rest-operands exps) env))))

    ;; The shape of a parameter defines the way
    ;; its corresponding argument is represented:
    ;;             x -> the actual value
    ;;      (x lazy) -> a thunk
    ;; (x lazy-memo) -> a memoized thunk
    (define (arg-exp->arg param arg-exp env)
      (cond ((symbol? param) (actual-value arg-exp env))
            ((lazy-param? param) (delay-it arg-exp env))
            ((lazy-memo-param? param) (delay-it-memo arg-exp env))
            (else
             (error "Unknown parameter type -- ARG-EXP->ARG" param))))
    
    (define (param-pair? param)
      (and (pair? param) (= (length param) 2)))
    
    (define (lazy-param? param)
      (and (param-pair? param) (eq? (cadr param) 'lazy)))

    (define (lazy-memo-param? param)
      (and (param-pair? param) (eq? (cadr param) 'lazy-memo)))

    (define (param-names params) (map param-name params))
    
    (define (param-name param)
      (cond ((symbol? param) param)
            ((param-pair? param) (car param))
            (else
             (error "Unknown parameter type -- PARAM-NAME" param))))

    (define (list-of-arg-values exps env)
      (if (no-operands? exps)
          '()
          (cons (actual-value (first-operand exps) env)
                (list-of-arg-values (rest-operands exps)
                                    env))))

    (define (list-of-delayed-args exps env)
      (if (no-operands? exps)
          '()
          (cons (delay-it-memo (first-operand exps) env)
                (list-of-delayed-args (rest-operands exps)
                                      env))))

    (define (actual-value exp env)
      (force-it (_eval exp env)))

    (define (force-it obj)
      (cond ((thunk? obj)
             (actual-value (thunk-exp obj) (thunk-env obj)))
            ((thunk-memo? obj)
             (evaluate-thunk! obj actual-value))
            ((evaluated-thunk? obj)
             (thunk-value obj))
            (else obj)))
    
    (define (self-evaluating? exp)
      (cond ((number? exp) true)
            ((string? exp) true)
            (else false)))

    (define (analyze-self-evaluating exp)
      (lambda (env) exp))

    (define (variable? exp) (symbol? exp))

    (define (analyze-variable exp)
      (lambda (env) (lookup-variable-value exp env)))

    (define (classify exp type)
      (if (pair? exp)
          (get type (car exp))
          false))

    (define (eval-sequence exps env)
      (cond ((last-exp? exps)
             ;; (actual-value (first-exp exps) env)) ; See: ex-4.30
             (_eval (first-exp exps) env))
            (else (actual-value (first-exp exps) env)
                  ;; (_eval (first-exp exps) env) ; See: ex-4-30
                  (eval-sequence (rest-exps exps) env))))

    (define (analyze-sequence exps)
      (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))
      (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
      (let ((procs (map _analyze exps)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE"))
        (loop (car procs) (cdr procs))))


    (define (application? exp) (pair? exp))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (define (no-operands? ops) (null? ops))
    (define (first-operand ops) (car ops))
    (define (rest-operands ops) (cdr ops))

    ;; Primitive procedure application
    (define (apply-primitive-procedure proc args)
      (apply (primitive-implementation proc) args))


    ;; Extensions support
    (define (extend-eval type proc)
      (put '_eval type proc))
    (define (extend-analyze type proc)
      (put '_analyze type proc))
    (define (def-constructor type proc)
      (put 'constructor type proc))
    (define (get-constructor type)
      (get 'constructor type))
    
    
    ;; Interface to the rest of the system
    (define (dispatch m)
      (cond ((eq? m '_eval) _eval)
            ((eq? m '_analyze) _analyze)
            ((eq? m '_apply) _apply)
            ((eq? m '_eval-seq) eval-sequence)
            ((eq? m '_analyze-seq) analyze-sequence)
            ((eq? m 'actual-value) actual-value)
            ((eq? m 'extend-eval) extend-eval)
            ((eq? m 'extend-analyze) extend-analyze)
            ((eq? m 'def-constructor) def-constructor)
            ((eq? m 'get-constructor) get-constructor)
            (else (error "Unknown operation -- MAKE-EVALUATOR" m))))

    dispatch))
