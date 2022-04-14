;; Interfacing Compiled Code to the Evaluator
;;
;; Requires the following definitions:
;; - ec-eval-machine (& start, set-register-contents!)
;; - compile (& statements)
;; - setup-environment
;;
;; Go to Explicit-Control Evaluator (evaluator-machine.scm) and
;; load the necessary definitions as stated there.
;;
;; Note: the following error might get thrown (depends on
;; whether exercise 5.9 was done or not):
;;
;; ;Operations can't be used with labels -- ASSEMBLE
;;
;; To get rid of this error, machine must be allowed
;; to operate on labels (make-operation-proc in assembler.scm).

(define (assemble-object-code object-code)
  (((ec-eval-machine 'assembler) 'assemble) object-code))

(define (compile-and-go exp)
  (let* ((object-code (statements (compile exp 'val 'return)))
         (instructions (assemble-object-code object-code)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! ec-eval-machine 'val instructions)
    (set-register-contents! ec-eval-machine 'flag true)
    (start ec-eval-machine)))
