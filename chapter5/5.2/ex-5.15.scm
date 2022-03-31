;; Exercise 5.15
;;
;; Add instruction counting to the register machine simulation.
;;
;; That is, have the machine model keep track of the number
;; of instructions executed.
;;
;; Extend the machine model's interface to accept a new message
;; that prints the value of the instruction count and
;; resets the count to zero.


;; Start by allocating memory for the instruction counter
;; in the basic machine:

(ic 0)

;; Next, extend the procedure execute to make it increment
;; instruction counter:

(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
        'done
        (begin
          ((instruction-execution-proc (car insts)))
          (set! ic (+ ic 1)) ; Increment the instruction counter
          (execute)))))

;; Next, install the following procedures as machine operations
;; and, optionally, as part of the machine external interface:

(define (print-ic)
  (newline)
  (display (list 'instruction-count '= ic)))

(define (reset-ic)
  (set! ic 0)
  'done)

;; Notice, it was decided to split the print and reset operations
;; in two.


;; Tests

(load "machine.scm")
(load "basic-machine-ext.scm")

;; Test utils

(define (print . items)
  (newline)
  (for-each (lambda (item)
              (display item))
            items))


;; Test: expt recursive

(define expt-rec-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *)
         (list 'read read) (list 'print print))
   '(expt-rec-init
     (perform (op reset-ic)) ; reset instruction count
     (assign continue (label expt-done))
     (perform (op print) (const "Enter b: "))
     (assign b (op read))
     (perform (op print) (const "Enter n: "))
     (assign n (op read))
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
     expt-done
     (perform (op print) (const "b^n = ") (reg val))
     (perform (op print-ic)) ; print instruction count
     (goto (label expt-rec-init)))))

(start expt-rec-machine)

;; 2^4 = 16
;; (instruction-count = 47)

;; 2^5 = 32
;; (instruction-count = 56)

;; 3^4 = 81
;; (instruction-count = 47)

;; 3^5 = 243
;; (instruction-count = 56)


;; Test: Factorial machine

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =) (list '- -) (list '* *)
         (list 'read read) (list 'print print))
   '(fact-init
     (perform (op reset-ic)) ; reset instruction count
     (assign continue (label fact-done))     ; set up final return address
     (perform (op print) (const "Enter n: "))
     (assign n (op read))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done
     (perform (op print) (const "n! = ") (reg val))
     (perform (op print-ic)) ; print instruction count
     (goto (label fact-init)))))

(start fact-machine)

;; 1 ; (instruction-count = 9)
;; 2 ; (instruction-count = 20)
;; 3 ; (instruction-count = 31)
;; 4 ; (instruction-count = 42)
;; 5 ; (instruction-count = 53)
