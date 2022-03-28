;; Restore Instruction
;;
;; The stack instruction 'restore'.

(load "register.scm")
(load "stack.scm")
(load "pc.scm")

(define (install-restore assembler)
  (let ((extend-assemble (assembler 'extend-assemble)))

    (define (assemble-restore inst machine labels)
      (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg ((machine 'get-register) reg-name))
              (stack (machine 'stack))
              ;; (stack ((machine 'register-stack) reg-name)) ; ex. 5.11 c.
              (pc (machine 'pc)))
          (lambda ()
            (set-contents! reg (pop stack))
            (advance-pc pc)))))

    (define (assemble-restore-b inst machine labels)
      (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg ((machine 'get-register) reg-name))
              (stack (machine 'stack))
              (pc (machine 'pc)))
          (lambda ()
            (let ((stack-record (pop stack)))
              (if (eq? (car stack-record) reg-name)
                  (begin
                    (set-contents! reg (cdr stack-record))
                    (advance-pc pc))
                  (error "Can't restore the register -- ASSEMBLE-RESTORE" reg-name)))))))

    (define (stack-inst-reg-name stack-instruction)
      (cadr stack-instruction))

    (extend-assemble 'restore assemble-restore)
    (extend-assemble 'restore-b assemble-restore-b)))
