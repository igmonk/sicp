;; Exercise 5.35
;;
;; What expression was compiled to produce the code shown below?

(assign val (op make-compiled-procedure) (label entry16)
        (reg env))
(goto (label after-lambda15))
entry16
(assign env (op compiled-procedure-env) (reg proc))
(assign env
        (op extend-environment) (const (x)) (reg argl) (reg env))
(assign proc (op lookup-variable-value) (const +) (reg env))
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const g) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 2))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
compiled-branch18
(assign continue (label after-call17))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch19
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch22))
compiled-branch21
(assign continue (label after-call20))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch22
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
(assign argl (op list) (reg val))
(restore env)
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch25))
compiled-branch24
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch25
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call23
after-lambda15
(perform (op define-variable!) (const f) (reg val) (reg env))
(assign val (const ok))


;; Below is line-by-line analysis of the compiled code.

;; The first two lines create a compiled procedure whose
;; entry point is entry16:
(assign val (op make-compiled-procedure) (label entry16)
        (reg env))
(goto (label after-lambda15))

;; The compiled procedure becomes the value of a new definition
;; named f and installed at the entry point after-lambda15:
after-lambda15
(perform (op define-variable!) (const f) (reg val) (reg env))
(assign val (const ok))

;; Hence, the source code might look as follows:
(define (f <params>)
  <...>
  )


;; To get an idea which parameters must be passed to f, analyse
;; what the procedure environment is extended with:
entry16
(assign env (op compiled-procedure-env) (reg proc))
(assign env
        (op extend-environment) (const (x)) (reg argl) (reg env))

;; Hence, f takes only one argument - x:
(define (f x)
  <...>
  )


;; Next comes a series of lookup-variable-value calls, whose
;; results get stored in the register proc.
;;
;; First comes the procedure +, after which the registers
;; continue, proc and env are saved onto the stack. That
;; indicates the procedure + takes more than one argument
;; and the next one to be evaluated is a procedure
;; (remember the evaluation order is right-to-left).
;;
;; Second comes the procedure g, after which the only register
;; that is saved onto the stack is proc, which suggests that
;; g takes only one argument, which is another procedure.
;;
;; Finally comes another proceduer + (inner) that takes
;; two arguments evaluated from right to left: 2 and x,
;; respectively.
(assign proc (op lookup-variable-value) (const +) (reg env))
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const g) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 2))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))

;; Thus, the following source code might be suggested:

(define (f x)
  (+ <arg1> (g (+ x 2)))
  )


;; Next comes the application of the inner +
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
compiled-branch18
(assign continue (label after-call17))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch19
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))


;; Next comes the application of g to the result of (+ x 2):
after-call17
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch22))
compiled-branch21
(assign continue (label after-call20))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch22
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))


;; Next follows the application of the outer +.
;; Here can be seen that the first argument to + is x.
;; Notice the compiled procedure branch that represents
;; a tail call (no assignment is required for continue).
after-call20
(assign argl (op list) (reg val))
(restore env)
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch25))
compiled-branch24
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch25
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))


;; Finally the original expression looks as follows:

(define (f x)
  (+ x (g (+ x 2))))


;; Compile it to see if that's correct:

(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-f
  (compile
   '(define (f x)
      (+ x (g (+ x 2))))
   'val
   'next))

(instruction-seq->file compiled-f "5.35.f.obj")
