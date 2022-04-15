;; Save Instruction
;;
;; The stack instruction 'save'.

(load "../5.2/register.scm")
(load "../5.2/stack.scm")
(load "../5.2/pc.scm")

(define (install-save assembler)
  (let ((extend-assemble (assembler 'extend-assemble)))

    (define (assemble-save inst machine labels)
      (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg ((machine 'get-register) reg-name))
              (stack (machine 'stack))
              ;; (stack ((machine 'register-stack) reg-name)) ; ex. 5.11 c.
              (pc (machine 'pc)))
          (lambda ()
            (push stack (get-contents reg))
            (advance-pc pc)))))

    (define (assemble-save-b inst machine labels)
      (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg ((machine 'get-register) reg-name))
              (stack (machine 'stack))
              (pc (machine 'pc)))
        (lambda ()
          (push stack (cons reg-name (get-contents reg)))
          (advance-pc pc)))))

    (define (stack-inst-reg-name stack-instruction)
      (cadr stack-instruction))

    (extend-assemble 'save assemble-save)
    (extend-assemble 'save-b assemble-save-b)))
