;; Compile and run
;;
;; A primitive that can be called from within
;; the explicit-control evaluator.


;; Since compile-and-run is designed to be applied as
;; a primitive procedure, examine the primitive-apply
;; entry point to see what comes right after the
;; procedure has been applied:
;;
;; primitive-apply
;; (assign val (op apply-primitive-procedure)
;;         (reg proc)
;;         (reg argl))
;; (restore continue)
;; (goto (reg continue))


;; The last two instructions restore the register continue
;; and unconditionally branch to what it refer to:
;;
;; (restore continue)
;; (goto (reg continue))
;;
;; Hence, we need to replace the top of the stack with
;; what needs to be executed next: the assembled instructions.

(define (compile-and-run exp)
  (let ((stack (ec-eval-ext-machine 'stack))
        (instructions
         (assemble-object-code
          (append (statements (compile exp 'val 'next))
                  '((goto (reg printres)))))))
    
    ;; Optionally remove the old value of continue from
    ;; the top of the stack (which is cleared in REPL anyway).
    (stack 'pop)

    ;; Push the assembled instructions for execution.
    ((stack 'push) instructions)))

(define (assemble-object-code object-code)
  (((ec-eval-ext-machine 'assembler) 'assemble) object-code))
