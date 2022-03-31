;; Exercise 5.16
;;
;; Augment the simulator to provide for instruction tracing.
;;
;; That is, before each instruction is executed, the simulator
;; should print the text of the instruction.
;;
;; Make the machine model accept trace-on and trace-off messages
;; to turn tracing on and off.


;; Allocate memory for the trace-on in the basic machine and
;; set it to false:

(trace-on false)

;; Next, install a new check into the procedure execute to
;; print the instruction text if tracing is ON:

(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
        'done
        (begin
          (when trace-on ; Print the instruction text if tracing is ON
            (print-inst-text (car insts)))
          ((instruction-execution-proc (car insts)))
          (set! ic (+ ic 1))
          (execute)))))

(define (print-inst-text inst)
  (newline)
  (display (list "Exec inst: " (instruction-text inst))))

;; Finally, set up the corresponding procedures to be invoked when
;; the machine model receives trace-on and trace-off messages:

((eq? message 'trace-on) (set! trace-on true))
((eq? message 'trace-off) (set! trace-on false))


;; Test: expt recursive

(load "machine.scm")
(load "basic-machine-ext.scm")

(define expt-rec-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(expt-rec-machine 'trace-on) ; Tracing is ON

(set-register-contents! expt-rec-machine 'b 2) ; done
(set-register-contents! expt-rec-machine 'n 4) ; done
(start expt-rec-machine)
;; (Exec inst:  (assign continue (label expt-done)))
;; (Exec inst:  (test (op =) (reg n) (const 0)))
;; (Exec inst:  (branch (label base-case)))
;; (Exec inst:  (save continue))
;; (Exec inst:  (assign n (op -) (reg n) (const 1)))
;; (Exec inst:  (assign continue (label after-expt)))
;; (Exec inst:  (goto (label expt-loop)))
;; (Exec inst:  (test (op =) (reg n) (const 0)))
;; (Exec inst:  (branch (label base-case)))
;; (Exec inst:  (save continue))
;; (Exec inst:  (assign n (op -) (reg n) (const 1)))
;; (Exec inst:  (assign continue (label after-expt)))
;; (Exec inst:  (goto (label expt-loop)))
;; (Exec inst:  (test (op =) (reg n) (const 0)))
;; (Exec inst:  (branch (label base-case)))
;; (Exec inst:  (save continue))
;; (Exec inst:  (assign n (op -) (reg n) (const 1)))
;; (Exec inst:  (assign continue (label after-expt)))
;; (Exec inst:  (goto (label expt-loop)))
;; (Exec inst:  (test (op =) (reg n) (const 0)))
;; (Exec inst:  (branch (label base-case)))
;; (Exec inst:  (save continue))
;; (Exec inst:  (assign n (op -) (reg n) (const 1)))
;; (Exec inst:  (assign continue (label after-expt)))
;; (Exec inst:  (goto (label expt-loop)))
;; (Exec inst:  (test (op =) (reg n) (const 0)))
;; (Exec inst:  (branch (label base-case)))
;; (Exec inst:  (assign val (const 1)))
;; (Exec inst:  (goto (reg continue)))
;; (Exec inst:  (restore continue))
;; (Exec inst:  (assign val (op *) (reg b) (reg val)))
;; (Exec inst:  (goto (reg continue)))
;; (Exec inst:  (restore continue))
;; (Exec inst:  (assign val (op *) (reg b) (reg val)))
;; (Exec inst:  (goto (reg continue)))
;; (Exec inst:  (restore continue))
;; (Exec inst:  (assign val (op *) (reg b) (reg val)))
;; (Exec inst:  (goto (reg continue)))
;; (Exec inst:  (restore continue))
;; (Exec inst:  (assign val (op *) (reg b) (reg val)))
;; (Exec inst:  (goto (reg continue)))
;; ;Value: done
(get-register-contents expt-rec-machine 'val)  ; 16


(expt-rec-machine 'trace-off) ; Tracing is OFF

(set-register-contents! expt-rec-machine 'b 2) ; done
(set-register-contents! expt-rec-machine 'n 4) ; done
(start expt-rec-machine)                       ; done
(get-register-contents expt-rec-machine 'val)  ; 16
