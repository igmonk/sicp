;; Exercise 5.34
;;
;; Compile the iterative factorial procedure

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Annotate the resulting code, showing the essential difference
;; between the code for iterative and recursive versions of factorial
;; that makes one process build up stack space and the other run in
;; constant stack space.


(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-factorial-iter
  (compile
   '(define (factorial n)
      (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
      (iter 1 1))
   'val
   'next))

(instruction-seq->file compiled-factorial-iter "factorial-iter.obj")


;; Below is the compiled code annotated with some comments:

(assign val (op make-compiled-procedure) (label entry2) (reg env))
(goto (label after-lambda1))
entry2 ; calls to factorial will entry here
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(assign val (op make-compiled-procedure) (label entry7) (reg env))
(goto (label after-lambda6))
entry7 ; calls to iter will entry here
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
(save continue)
(save env)
(assign proc (op lookup-variable-value) (const >) (reg env))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch22))
compiled-branch21
(assign continue (label after-call20))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch22
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
(restore env)
(restore continue)
(test (op false?) (reg val))
(branch (label false-branch9))
true-branch10
(assign val (op lookup-variable-value) (const product) (reg env))
(goto (reg continue))
;; Resolving the procedure iter to be recursively invoked later
;; and saving the registers continue, proc and env onto the stack:
false-branch9
(assign proc (op lookup-variable-value) (const iter) (reg env))
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch16))
compiled-branch15
(assign continue (label after-call14))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch16
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; Restoring env after the procedure + call (the register env
;; will not be saved again, since the code is about to evaluate
;; the last argument to iter):
after-call14
(assign argl (op list) (reg val))
(restore env)
(save argl)
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (op lookup-variable-value) (const product) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch13))
compiled-branch12
(assign continue (label after-call11))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch13
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; Restoring argl, proc and continue after the procedure * call:
after-call11
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
;; Subsequent recursive invocation of iter, which is a proper
;; tail-call optimization.
;; Note, the stack size is the same as it was before the call to
;; the current iteration.
compiled-branch18
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch19
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call17
after-if8
after-lambda6
(perform (op define-variable!) (const iter) (reg val) (reg env))
(assign val (const ok))
(assign proc (op lookup-variable-value) (const iter) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 1))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch5))
compiled-branch4
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch5
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call3
after-lambda1
(perform (op define-variable!) (const factorial) (reg val) (reg env))
(assign val (const ok))
