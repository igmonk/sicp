;; Exercise 5.47
;;
;; This section described how to modify the explicit-control evaluator
;; so that interpreted code can call compiled procedures.
;;
;; Show how to modify the compiler so that compiled procedures
;; can call not only primitive procedures and compiled procedures,
;; but interpreted procedures as well.
;;
;; This requires modifying compile-procedure-call to handle the case
;; of compound (interpreted) procedures.
;;
;; Be sure to handle all the same target and linkage combinations
;; as in compile-proc-appl.
;;
;; To do the actual procedure application, the code needs to jump to
;; the evaluator's compound-apply entry point. This label cannot be
;; directly referenced in object code (since the assembler requires
;; that all labels referenced by the code it is assembling be defined
;; there), so we will add a register called compapp to the evaluator
;; machine to hold this entry point, and add an instruction to
;; initialize it:

(assign compapp (label compound-apply))
(branch (label external-entry))      ; branches if flag is set
read-eval-print-loop
<...>

;; To test your code, start by defining a procedure f that calls
;; a procedure g. Use compile-and-go to compile the definition of f
;; and start the evaluator.
;;
;; Now, typing at the evaluator, define g and try to call f.


(load "compiler-factory.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(load "compile-and-go.scm")


;; First, let's inspect the errors thrown by current implementation.

(compile-and-go
 '(define (f x)
    (* 10 (g x))))

;;; EC-Eval input:

(f 5) ; Unbound variable g

;; The same but with the definition of g:

(define (g n)
  (+ n 2))

;;; EC-Eval input:

(f 5) ; The object n, passed as the first argument to cdr, is not the correct type


;; Let's examine the excerpt from the sequence of compiled
;; instructions to get an idea of what's going on.

(load "io-utils.scm")

(instruction-seq->file
 (compile
  '(define (f x)
     (* 10 (g x)))
  'val
  'next)
 "5.47.f.obj")

;; An error is thrown since the compiler does not arrange for
;; compound (interpreted) procedures, and when one comes about
;; it treats such a procedure as a compiled one.

;; The following sequence of compiled instructions shows
;; that the compound procedure's parameters are being treated
;; as a compiled procedure entry, which the interpreter is to
;; go to:

<...>
compiled-branch4
(assign continue (label after-call3))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
<...>


;; Finally, comes the support of compound procedures in
;; the compiler.


;; compile-procedure-call
;;
;; The updated procedure-application code has the following form:
;;
;;  (test (op primitive-procedure?) (reg proc))
;;  (branch (label primitive-branch))
;;  (test (op compiled-procedure?) (reg proc))
;;  (branch (label compiled-branch))
;; compound-branch
;;  <code to apply compound procedure with given target and appropriate linkage>
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
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((non-primitive-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compiled-procedure?) (reg proc))
          (branch (label ,compiled-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compound-branch
         (compound-proc-appl target non-primitive-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compiled-branch
          (compile-proc-appl target non-primitive-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage
           linkage
           (make-instruction-sequence
            '(proc argl) (list target)
            `((assign ,target
                      (op apply-primitive-procedure)
                      (reg proc)
                      (reg argl))))))))
       after-call))))


;; compound-proc-appl
;;
;; The procedure handles the case of compound (interpreted)
;; procedures and is reminiscent of compile-proc-appl.
;;
;; Notice the register continue is saved onto the stack
;; before branching to the compound-apply entry point,
;; which tells where to return with the result of the
;; procedure application.
;;
;; When the application of a compound procedure is complete,
;; the controller transfers to the entry point specified by
;; the saved continue, with the result of the application
;; in val (see section 5.4).

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          `((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))


;; Tests

(compile-and-go
 '(define (f x)
    (* 10 (g x))))

;;; EC-Eval input:

(define (g n)
  (+ n 2))

;;; EC-Eval input:

(f 5) ; 70
(f 6) ; 80
(f 7) ; 90
