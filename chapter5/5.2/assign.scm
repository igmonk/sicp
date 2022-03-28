;; Assign Instruction
;;
;; The execution procedure for the 'assign' instruction
;; sets the contents of the target register to the result
;; obtained by executing 'value-proc' - a procedure of
;; no arguments which will be evaluated during the simulation
;; to produce the actual value to be assigned to the register.

(load "register.scm")
(load "operation.scm")
(load "pc.scm")

(define (install-assign assembler)
  (let ((extend-assemble (assembler 'extend-assemble))
        (make-operation-proc (assembler 'make-operation-proc))
        (make-primitive-proc (assembler 'make-primitive-proc)))

    (define (assemble-assign inst machine labels)
      (let ((target ((machine 'get-register)
                     (assign-reg-name inst)))
            (value-exp (assign-value-exp inst))
            (pc (machine 'pc)))
        (let ((value-proc
               (if (operation-exp? value-exp)
                   (make-operation-proc value-exp labels)
                   (make-primitive-proc (car value-exp) labels))))
          (lambda ()
            (set-contents! target (value-proc))
            (advance-pc pc)))))

    (define (assign-reg-name assign-instruction)
      (cadr assign-instruction))

    (define (assign-value-exp assign-instruction)
      (cddr assign-instruction))

    (extend-assemble 'assign assemble-assign)))
