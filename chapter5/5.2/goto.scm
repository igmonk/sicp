;; GoTo Instruction
;;
;; Similar to a branch, except that the destination may be
;; specified either as a label or as a register, and
;; there is no condition to check - the pc is always
;; set to the new destination.

(load "../5.2/register.scm")
(load "../5.2/label.scm")
(load "../5.2/pc.scm")

(define (install-goto assembler)
  (let ((extend-assemble (assembler 'extend-assemble))
        (lookup-label (assembler 'lookup-label)))

    (define (assemble-goto inst machine labels)
      (let ((dest (goto-dest inst))
            (pc (machine 'pc)))
        (cond ((label-exp? dest)
               (let ((insts
                      (lookup-label labels (label-exp-label dest))))
                 (lambda () (set-contents! pc insts))))
              ((register-exp? dest)
               (let ((reg ((machine 'get-register)
                           (register-exp-reg dest))))
                 (lambda ()
                   (set-contents! pc (get-contents reg)))))
              (else
               (error "Bad GOTO instruction -- ASSEMBLE-GOTO" inst)))))

    (define (goto-dest goto-instruction)
      (cadr goto-instruction))

    (extend-assemble 'goto assemble-goto)))
