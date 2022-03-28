;; The basic machine
;;
;; The make-new-machine procedure constructs an object
;; whose local state consists of:
;; - a stack
;; - an initially empty instruction sequence
;; - a list of operations that initially contains
;;   an operation to initialize the stack
;; - a register table that initially contains two registers,
;;   named 'flag' and 'pc' (for "program counter")
;;
;; The flag register is used to control branching.
;;
;; 'test' instructions set the contents of flag to
;; the result of the test (true or false).
;;
;; 'branch' instructions decide whether or not to branch
;; by examining the contents of flag.
;;
;; The pc register determines the sequencing of instructions
;; as the machine runs.
;;
;; In the simulation model, each machine instruction is
;; a data structure that includes a procedure of no arguments,
;; called the 'instruction execution procedure', such that
;; calling this procedure simulates executing the instruction.
;;
;; As the simulation runs, pc points to the place in
;; the instruction sequence beginning with the next instruction
;; to be executed.
;;
;; As part of its operation, each instruction execution procedure
;; modifies pc to indicate the next instruction to be executed.
;;
;; Branch and goto instructions change pc to point to
;; the new destination.
;;
;; All other instructions simply advance pc, making it point to
;; the next instruction in the sequence.

(load "register.scm")
(load "stack.scm")
(load "instruction.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (ic 0) ; instruction count
        (trace-on false) ; instruction tracing
        ;; (stacks '()) ; ex. 5.11 c.
        (the-instruction-sequence '()))

    (define (print-ic)
      (newline)
      (display (list 'instruction-count '= ic)))

    (define (reset-ic)
      (set! ic 0)
      'done)
    
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))
                 (list 'print-ic print-ic)
                 (list 'reset-ic reset-ic)))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))
              ;; (set! stacks ; ex. 5.11 c.
              ;;       (cons (list name (make-stack))
              ;;             stacks))
              )))

      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))

      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (when trace-on
                  (print-inst-text (car insts)))
                ((instruction-execution-proc (car insts)))
                (set! ic (+ ic 1))
                (execute)))))

      (define (print-inst-text inst)
        (newline)
        (display (list "Exec inst: " (instruction-text inst))))

      (define (register-stack reg-name)
        (let ((stack (assoc reg-name stacks)))
          (if stack
              (cadr stack)
              (error "Can't find stack for the given register -- MACHINE"
                     reg-name))))

      (define (reg-trace-on reg-name)
        ((lookup-register reg-name) 'trace-on))

      (define (reg-trace-off reg-name)
        ((lookup-register reg-name) 'trace-off))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ;; ((eq? message 'register-stack) register-stack) ; ex. 5.11 c.
              ((eq? message 'operations) the-ops)
              ((eq? message 'pc) pc)
              ((eq? message 'flag) flag)
              ((eq? message 'ic) ic)
              ((eq? message 'reset-ic) (reset-ic))
              ((eq? message 'trace-on) (set! trace-on true))
              ((eq? message 'trace-off) (set! trace-on false))
              ((eq? message 'reg-trace-on) reg-trace-on)
              ((eq? message 'reg-trace-off) reg-trace-off)
              (else
               (error "Unknown request -- MACHINE" message))))
      
      dispatch)))
