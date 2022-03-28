;; Test Instruction
;;
;; The procedure make-test handles 'test' instructions.
;; It extracts the expression that specifies the condition
;; to be tested and generates an execution procedure for it.
;;
;; At simulation time, the procedure for the condition is called,
;; the result is assigned to the flag register, and
;; the pc is advanced.

(load "register.scm")
(load "operation.scm")
(load "pc.scm")

(define (install-test assembler)
  (let ((extend-assemble (assembler 'extend-assemble))
        (make-operation-proc (assembler 'make-operation-proc)))

    (define (assemble-test inst machine labels)
      (let ((condition (test-condition inst))
            (flag (machine 'flag))
            (pc (machine 'pc)))
        (if (operation-exp? condition)
            (let ((condition-proc
                   (make-operation-proc condition labels)))
              (lambda ()
                (set-contents! flag (condition-proc))
                (advance-pc pc)))
            (error "Bad TEST instruction -- ASSEMBLE-TEST" inst))))

    (define (test-condition test-instruction)
      (cdr test-instruction))

    (extend-assemble 'test assemble-test)))
