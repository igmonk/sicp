;; Perform Instruction
;;
;; Generates an execution procedure for the action to be performed.
;; At simulation time, the action procedure is executed and
;; the pc advanced.

(load "operation.scm")
(load "pc.scm")

(define (install-perform assembler)
  (let ((extend-assemble (assembler 'extend-assemble))
        (make-operation-proc (assembler 'make-operation-proc)))

    (define (assemble-perform inst machine labels)
      (let ((action (perform-action inst))
            (pc (machine 'pc)))
        (if (operation-exp? action)
            (let ((action-proc
                   (make-operation-proc action labels)))
              (lambda ()
                (action-proc)
                (advance-pc pc)))
            (error "Bad PERFORM instruction -- ASSEMBLE-PERFORM" inst))))

    (define (perform-action inst) (cdr inst))

    (extend-assemble 'perform assemble-perform)))
