;; Exercise 5.11
;;
;; When we introduced save and restore in section 5.1.4,
;; we didn't specify what would happen if you tried to
;; restore a register that was not the last one saved,
;; as in the sequence
;;
;; (save y)
;; (save x)
;; (restore y)
;;
;; There are several reasonable possibilities for the meaning
;; of restore:
;;
;; a. (restore y) puts into y the last value saved on the stack,
;;    regardless of what register that value came from. This is
;;    the way our simulator behaves.
;;    Show how to take advantage of this behavior to eliminate
;;    one instruction from the Fibonacci machine of section 5.1.4
;;    (figure 5.12).
;;
;; b. (restore y) puts into y the last value saved on the stack,
;;    but only if that value was saved from y;
;;    otherwise, it signals an error.
;;    Modify the simulator to behave this way.
;;    You will have to change save to put the register name
;;    on the stack along with the value.
;;
;; c. (restore y) puts into y the last value saved from y regardless
;;    of what other registers were saved after y and not restored.
;;    Modify the simulator to behave this way.
;;    You will have to associate a separate stack with each register.
;;    You should make the initialize-stack operation initialize all
;;    the register stacks.


;; a. The following instruction sequence

afterfib-n-2                         ; upon return, val contains Fib(n - 2)
  (assign n (reg val))               ; n now contains Fib(n - 2)
  (restore val)                      ; val now contains Fib(n - 1)
  (restore continue)
  (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
          (op +) (reg val) (reg n))
  (goto (reg continue))              ; return to caller, answer is in val

;; can be simplified to

afterfib-n-2                         ; upon return, val contains Fib(n - 2)
  (restore n)                        ; n nwo contains val = Fib(n - 1)
  (restore continue)
  (assign val                        ;  Fib(n - 2) +  Fib(n - 1)
          (op +) (reg val) (reg n))
  (goto (reg continue))              ; return to caller, answer is in val

;; since what's been placed on top of the stack, gets restored
;; to the register n, and then is added to the value of val.
;;
;; Hence, the sum doesn't change since the operation is given
;; the same values albeit in the opposite order.


(load "machine.scm")
(load "basic-machine-ext.scm")

;; b. restore-b instruction
;;
;; The following save/restore procedures have been introduced
;; and installed into the assembler:

(define (assemble-save-b inst machine labels)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg ((machine 'get-register) reg-name))
          (stack (machine 'stack))
          (pc (machine 'pc)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(extend-assemble 'save-b assemble-save-b)

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

(extend-assemble 'restore-b assemble-restore-b)


;; Test

(define restore-b-test-machine
  (make-machine
   '(x y)
   '()
   '((assign x (const 1))
     (assign y (const 2))
     (save-b x)
     (save-b y)
     (restore-b x)
     done)))

;; (start restore-b-test-machine) ; Can't restore the register -- ASSEMBLE-RESTORE x


;; c. The original save/restore instructions has been modified
;;    to search for a register-related stack:

(stack ((machine 'register-stack) reg-name))

;; The following changes have been applied to make-new-machine:
;;
;; 1. An association list for register-related stacks:

(stacks '())

;; 2. allocate-register

(define (allocate-register name)
  (if (assoc name register-table)
      (error "Multiply defined register: " name)
      (begin
        (set! register-table
              (cons (list name (make-register name))
                    register-table))
        (set! stacks ;; ex. 5.11 c.
              (cons (list name (make-stack))
                    stacks)))))

;; 3. A procedure used to find a stack by register name:

(define (register-stack reg-name)
  (let ((stack (assoc reg-name stacks)))
    (if stack
        (cadr stack)
        (error "Can't find stack for the given register -- MACHINE"
               reg-name))))

;; 4. dispatch extension:

((eq? message 'register-stack) register-stack)


;; Test

(define c-test-machine
  (make-machine
   '(x y z)
   '()
   '((save x)
     (save y)
     (save z)
     (restore x)
     (restore y)
     (restore z)
     done)))


;; Set the register values and restore them in the order
;; opposite to FIFO:

(set-register-contents! c-test-machine 'x 1) ; done
(set-register-contents! c-test-machine 'y 2) ; done
(set-register-contents! c-test-machine 'z 3) ; done

(start c-test-machine) ; done

(get-register-contents c-test-machine 'x) ; 1
(get-register-contents c-test-machine 'y) ; 2
(get-register-contents c-test-machine 'z) ; 3

;; As shown, each register was restored to the value that
;; had been stored in the corresponding independent stack.
