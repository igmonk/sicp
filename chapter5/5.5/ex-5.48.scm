;; Exercise 5.48
;;
;; The compile-and-go interface implemented in this section
;; is awkward, since the compiler can be called only once
;; (when the evaluator machine is started).
;;
;; Augment the compiler-interpreter interface by providing
;; a compile-and-run primitive that can be called from within
;; the explicit-control evaluator as follows:

;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))
;;; EC-Eval value:
ok
;;; EC-Eval input:
(factorial 5)
;;; EC-Eval value:
120


;; This functionality can be installed either as a special form
;; or as a primitive procedure. The latter approach is chosen.

(load "../5.2/basic-machine-ext.scm")
(load "../5.4/env-ext.scm")
(load "eval-machine.scm")
(load "instruction-seq.scm")


;; Firsly, add a new register called 'printres' to the evaluator
;; machine to reference the print-result entry point.
;; 
;; (the assembler requires that all labels referenced by the
;; code it is assembling be defined there).


;; Next, add the following instruction to initialize the
;; newly added register:

(assign printres (label print-result))

;; Next, implement a new procedure to be executed as a primitive
;; procedure by the evaluator machine.
;;
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


;; Add compile-and-run to the list of primitive procedures
;; reset the global environment, and run the machine to
;; reinitialize the machine's primitive procedures:

(append! primitive-procedures
         (list (list 'compile-and-run compile-and-run)))

(reset-env!)


;; Finally, set flag to false and run the evaluator machine:

(set-register-contents! ec-eval-ext-machine 'flag false)
(start ec-eval-ext-machine)

;;; EC-Eval input

;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(total-pushes = 6 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 31 maximum-depth = 14)
;;; EC-Eval value:
120

;;; EC-Eval input:
(compile-and-run
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(total-pushes = 6 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 10)

(total-pushes = 887 maximum-depth = 29)
;;; EC-Eval value:
55

;;; EC-Eval input:
