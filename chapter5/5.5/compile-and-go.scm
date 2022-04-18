;; Interfacing Compiled Code to the Evaluator

(load "../5.2/basic-machine-ext.scm")

(load "../5.4/env-ext.scm")

(load "eval-machine.scm")
(load "instruction-seq.scm")

;; Note: the following error might get thrown (depends on
;; whether exercise 5.9 was done or not):
;;
;; ;Operations can't be used with labels -- ASSEMBLE
;;
;; To get rid of this error, machine must be allowed to
;; operate on labels (see make-operation-proc in
;; ../5.2/assembler.scm).

(define (assemble-object-code object-code)
  (((ec-eval-ext-machine 'assembler) 'assemble) object-code))

;; Requires 'compile' to be defined
(define (compile-and-go exp)
  (let* ((object-code (statements (compile exp 'val 'return)))
         (instructions (assemble-object-code object-code)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! ec-eval-ext-machine 'val instructions)
    (set-register-contents! ec-eval-ext-machine 'flag true)
    (start ec-eval-ext-machine)))
