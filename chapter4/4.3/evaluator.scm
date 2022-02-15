;; Amb Evaluator
;;
;; The Amb evaluator whose dispatch is done in data-directed style.
;;
;; The evaluation process is be described as the interplay between
;; two procedures: 'eval' and 'apply'.
;;
;; In order not to mess up with the built-in eval and apply procedures,
;; their counterparts are to be prefixed with the underscore symbol.

(load "list-utils.scm")
(load "environment.scm")
(load "procedure.scm")

(define (make-evaluator edt)
  (let ((get (edt 'lookup-proc))
        (put (edt 'insert-proc!)))
    
    ;; Amb eval with preceding syntactic analysis
    (define (ambeval exp env succeed fail)
      ((_analyze exp) env succeed fail))
    
    ;; Eval mixed with syntactic analysis
    ;; (define (_eval exp env)
    ;;   (cond ((self-evaluating? exp) exp)
    ;;         ((variable? exp) (lookup-variable-value exp env))
    ;;         ((classify exp '_eval) ((classify exp '_eval) exp env))
    ;;         ((application? exp)
    ;;          (_apply (_eval (operator exp) env)
    ;;                  (list-of-values (operands exp) env)))
    ;;         (else
    ;;          (error "Unknown expression type -- EVAL" exp))))
    ;;
    ;; (define (_apply procedure arguments)
    ;;   (cond ((primitive-procedure? procedure)
    ;;          (apply-primitive-procedure procedure arguments))
    ;;         ((compound-procedure? procedure)
    ;;          (eval-sequence
    ;;           (procedure-body procedure)
    ;;           (extend-environment
    ;;            (procedure-parameters procedure)
    ;;            arguments
    ;;            (procedure-environment procedure))))
    ;;         (else
    ;;          (error "Unknown procedure type -- APPLY" procedure))))

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
        (lambda (env succeed fail)
          (fproc env
                 (lambda (proc fail2)
                   (get-args aprocs
                             env
                             (lambda (args fail3)
                               (execute-application proc args succeed fail3))
                             fail2))
                 fail))))

    (define (get-args aprocs env succeed fail)
      (if (null? aprocs)
          (succeed '() fail)
          ((car aprocs)
           env
           ;; success continuation for this aproc
           (lambda (arg fail2)
             (get-args (cdr aprocs)
                       env
                       ;; success continuation for recursive
                       (lambda (args fail3)
                         (succeed (cons arg args)
                                  fail3))
                       fail2))
           fail)))

    (define (execute-application proc args succeed fail)
      (cond ((primitive-procedure? proc)
             (succeed (apply-primitive-procedure proc args)
                      fail))
            ((compound-procedure? proc)
             ((procedure-body proc)
              (extend-environment
               (procedure-parameters proc)
               args
               (procedure-environment proc))
              succeed
              fail))
            (else
             (error "Unknown procedure type -- EXECUTE-APPLICATION"
                    proc))))

    (define (list-of-values exps env)
      (if (no-operands? exps)
          '()
          (cons (_eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

    (define (self-evaluating? exp)
      (cond ((number? exp) true)
            ((string? exp) true)
            (else false)))

    (define (analyze-self-evaluating exp)
      (lambda (env succeed fail)
        (succeed exp fail)))

    (define (variable? exp) (symbol? exp))

    (define (analyze-variable exp)
      (lambda (env succeed fail)
        (succeed (lookup-variable-value exp env)
                 fail)))

    (define (classify exp type)
      (if (pair? exp)
          (get type (car exp))
          false))

    (define (eval-sequence exps env)
      (cond ((last-exp? exps)
             (_eval (first-exp exps) env))
            (else (_eval (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))

    (define (analyze-sequence exps)
      (define (sequentially proc1 proc2)
        (lambda (env succeed fail)
          (proc1 env
                 ;; success continuation for calling a
                 (lambda (proc1-value fail2)
                   (proc2 env succeed fail2))
                 ;; failure continuation for calling a
                 fail)))
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
      (cond ((eq? m '_eval) "UNDEFINED. Use ambeval instead.")
            ((eq? m '_analyze) _analyze)
            ((eq? m '_apply) _apply)
            ((eq? m '_eval-seq) eval-sequence)
            ((eq? m '_analyze-seq) analyze-sequence)
            ((eq? m 'ambeval) ambeval)
            ((eq? m 'extend-eval) extend-eval)
            ((eq? m 'extend-analyze) extend-analyze)
            ((eq? m 'def-constructor) def-constructor)
            ((eq? m 'get-constructor) get-constructor)
            (else (error "Unknown operation -- MAKE-EVALUATOR" m))))

    dispatch))
