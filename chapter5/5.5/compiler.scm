;; Compiler

(load "../../chapter3/3.3/table-obj-2d.scm")

(load "list-utils.scm")
(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (make-compiler)
  (let ((dt (make-table))
        (label-counter 0))

    (define get (dt 'lookup-proc))
    (define put (dt 'insert-proc!))

    (define (new-label-number)
      (set! label-counter (+ 1 label-counter))
      label-counter)

    (define (make-label name)
      (string->symbol
       (string-append (symbol->string name)
                      (number->string (new-label-number)))))

    (define (compile exp target linkage)
      (cond ((self-evaluating? exp)
             (compile-self-evaluating exp target linkage))
            ((variable? exp)
             (compile-variable exp target linkage))
            ((classify exp 'compile)
             ((classify exp 'compile) exp target linkage))
            ((application? exp)
             (compile-application exp target linkage))
            (else
             (error "Unknown expression type -- COMPILE" exp))))

    (define (classify exp type)
      (if (pair? exp)
          (get type (car exp))
          false))

    (define (compile-linkage linkage)
      (cond ((eq? linkage 'return)
             (make-instruction-sequence
              '(continue) '()
              '((goto (reg continue)))))
            ((eq? linkage 'next)
             (empty-instruction-sequence))
            (else
             (make-instruction-sequence
              '() '()
              `((goto (label ,linkage)))))))

    (define (end-with-linkage linkage instruction-sequence)
      (preserving '(continue)
                  instruction-sequence
                  (compile-linkage linkage)))

    (define (compile-self-evaluating exp target linkage)
      (end-with-linkage
       linkage
       (make-instruction-sequence
        '() (list target)
        `((assign ,target (const ,exp))))))

    (define (compile-variable exp target linkage)
      (end-with-linkage
       linkage
       (make-instruction-sequence
        '(env) (list target)
        `((assign ,target
                  (op lookup-variable-value)
                  (const ,exp)
                  (reg env))))))

    (define (compile-sequence seq target linkage)
      (if (last-exp? seq)
          (compile (first-exp seq) target linkage)
          (preserving
           '(env continue)
           (compile (first-exp seq) target 'next)
           (compile-sequence (rest-exps seq) target linkage))))

    
    ;; The code for a combination compiled with a given target
    ;; and linkage has the form
    ;;
    ;; <compilation of operator, target proc, linkage next>
    ;; <evaluate operands and construct argument list in argl>
    ;; <compilation of procedure call with given target and linkage>
    ;;
    ;; The env register must be preserved around the evaluation of
    ;; the operator (since evaluating the operator might modify env, which
    ;; will be needed to evaluate the operands), and the proc register
    ;; must be preserved around the construction of the argument list
    ;; (since evaluating the operands might modify proc, which will be
    ;; needed for the actual procedure application).
    ;; continue must also be preserved throughout, since it is needed for
    ;; the linkage in the procedure call.
    (define (compile-application exp target linkage)
      (let ((proc-code (compile (operator exp) 'proc 'next))
            (operand-codes
             (map (lambda (operand) (compile operand 'val 'next))
                  (operands exp))))
        (preserving
         '(env continue)
         proc-code
         (preserving
          '(proc continue)
          (construct-arglist operand-codes)
          (compile-procedure-call target linkage)))))


    ;; The general form of the argument-list construction is as follows:
    ;;
    ;; <compilation of last operand, targeted to val>
    ;; (assign argl (op list) (reg val))
    ;; <compilation of next operand, targeted to val>
    ;; (assign argl (op cons) (reg val) (reg argl))
    ;; ...<compilation of first operand, targeted to val>
    ;; (assign argl (op cons) (reg val) (reg argl))
    ;;
    ;; - argl must be preserved around each operand evaluation except
    ;;   the first (so that arguments accumulated so far won't be lost)
    ;; - env must be preserved around each operand evaluation except
    ;;   the last (for use by subsequent operand evaluations).
    ;;
    ;; If there are no operands at all, it simply emits the instruction
    ;; (assign argl (const ()))
    (define (construct-arglist operand-codes)
      (let ((operand-codes (reverse operand-codes)))
        (if (null? operand-codes)
            (make-instruction-sequence
             '() '(argl)
             `((assign argl (const()))))
            (let ((code-to-get-last-arg
                   (append-instruction-sequences
                    (car operand-codes)
                    (make-instruction-sequence
                     '(val) '(argl)
                     `((assign argl (op list) (reg val)))))))
              (if (null? (cdr operand-codes))
                  code-to-get-last-arg
                  (preserving '(env)
                              code-to-get-last-arg
                              (code-to-get-rest-args
                               (cdr operand-codes))))))))

    (define (code-to-get-rest-args operand-codes)
      (let ((code-for-next-arg
             (preserving '(argl)
                         (car operand-codes)
                         (make-instruction-sequence
                          '(val argl) '(argl)
                          `((assign argl
                                    (op cons) (reg val) (reg argl)))))))
        (if (null? (cdr operand-codes))
            code-for-next-arg
            (preserving '(env)
                        code-for-next-arg
                        (code-to-get-rest-args (cdr operand-codes))))))


    ;; Applying procedures
    ;;
    ;; After evaluating the elements of a combination, the compiled code
    ;; must apply the procedure in proc to the arguments in argl.
    ;;
    ;; The procedure-application code has the following form:
    ;;
    ;;  (test (op primitive-procedure?) (reg proc))
    ;;  (branch (label primitive-branch))
    ;; compiled-branch
    ;;  <code to apply compiled procedure with given target and appropriate linkage>
    ;; primitive-branch
    ;;  (assign <target>
    ;;          (op apply-primitive-procedure)
    ;;          (reg proc)
    ;;          (reg argl))
    ;;  <linkage>
    ;; after-call
    (define (compile-procedure-call target linkage)
      (let ((primitive-branch (make-label 'primitive-branch))
            (compiled-branch (make-label 'compiled-branch))
            (after-call (make-label 'after-call)))
        (let ((compiled-linkage
               (if (eq? linkage 'next) after-call linkage)))
          (append-instruction-sequences
           (make-instruction-sequence
            '(proc) '()
            `((test (op primitive-procedure?) (reg proc))
              (branch (label ,primitive-branch))))
           (parallel-instruction-sequences
            (append-instruction-sequences
             compiled-branch
             (compile-proc-appl target compiled-linkage))
            (append-instruction-sequences
             primitive-branch
             (end-with-linkage
              linkage
              (make-instruction-sequence
               '(proc argl) (list target)
               `((assign ,target
                         (op apply-primitive-procedure)
                         (reg proc)
                         (reg argl)))))))
           after-call))))


    ;; Applying compiled procedures
    ;;
    ;; A compiled procedure has an entry point, which is a label
    ;; that designates where the code for the procedure starts.
    ;;
    ;; The code at this entry point computes a result in 'val'
    ;; and returns by executing the instruction (goto (reg continue)).
    ;;
    ;; With the 'return' linkage implementation, the compiler generates
    ;; tail-recursive code. Calling a procedure as the final step in
    ;; a procedure body does a direct transfer, without saving any
    ;; information on the stack.
    ;;
    ;; Generates the code by considering four cases, depending on
    ;; whether the target for the call is 'val' and whether
    ;; the linkage is 'return'.
    ;;
    ;; The instruction sequences are declared to modify all the registers,
    ;; since executing the procedure body can change the registers in
    ;; arbitrary ways.
    (define (compile-proc-appl target linkage)
      (cond ((and (eq? target 'val)
                  (not (eq? linkage 'return)))
             (make-instruction-sequence
              '(proc) all-regs
              `((assign continue (label ,linkage))
                (assign val (op compiled-procedure-entry) (reg proc))
                (goto (reg val)))))
            ((and (not (eq? target 'val))
                  (not (eq? linkage 'return)))
             (let ((proc-return (make-label 'proc-return)))
               (make-instruction-sequence
                '(proc) all-regs
                `((assign continue (label ,proc-return))
                  (assign val (op compiled-procedure-entry) (reg proc))
                  (goto (reg val))
                  ,proc-return
                  (assign ,target (reg val))
                  (goto (label ,linkage))))))
            ((and (eq? target 'val)
                  (eq? linkage 'return))
             (make-instruction-sequence
              '(proc continue) all-regs
              `((assign val (op compiled-procedure-entry) (reg proc))
                (goto (reg val)))))
            ((and (not (eq? target 'val))
                  (eq? linkage 'return))
             (error "return linkage, target not val -- COMPILE"
                    target))))
    
    (define all-regs '(env proc val argl continue))

    (define (application? exp) (pair? exp))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))

    (define (extend-compile type proc)
      (put 'compile type proc))

    (define (dispatch m)
      (cond ((eq? m 'compile) compile)
            ((eq? m 'extend-compile) extend-compile)
            ((eq? m 'end-with-linkage) end-with-linkage)
            ((eq? m 'compile-sequence) compile-sequence)
            ((eq? m 'make-label) make-label)
            (else
             (error "Unknown operation -- MAKE-COMPILER" m))))

    dispatch))