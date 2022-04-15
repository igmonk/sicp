;; The machine model
;;
;; The machine model is represented as a procedure with local state
;; using the message-passing techniques.
;;
;; make-machine extends the basic model (by sending it messages)
;; to include the registers, operations, and controller of
;; the particular machine being defined.

(load "../5.2/basic-machine.scm")
(load "../5.2/assembler.scm")

(load "../5.2/assign.scm")
(load "../5.2/test.scm")
(load "../5.2/branch.scm")
(load "../5.2/goto.scm")
(load "../5.2/save.scm")
(load "../5.2/restore.scm")
(load "../5.2/perform.scm")

(define (make-machine register-names ops controller-text)
  (let* ((machine (make-new-machine))
         (assembler (make-assembler machine)))
    
    ;; Install the special forms / instructions
    (install-assign assembler)
    (install-test assembler)
    (install-branch assembler)
    (install-goto assembler)
    (install-save assembler)
    (install-restore assembler)
    (install-perform assembler)
    
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     ((assembler 'assemble) controller-text))

    (define (dispatch message)
      (cond ((eq? message 'assembler) assembler)
            (else (machine message))))

    dispatch))
