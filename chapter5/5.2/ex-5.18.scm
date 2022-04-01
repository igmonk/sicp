;; Exercise 5.18
;;
;; Modify the make-register procedure of section 5.2.1 so that
;; registers can be traced.
;;
;; Registers should accept messages that turn tracing on and off.
;;
;; When a register is traced, assigning a value to the register
;; should print
;; - the name of the register
;; - the old contents of the register
;; - the new contents being assigned.
;;
;; Extend the interface to the machine model to permit you to
;; turn tracing on and off for designated machine registers.


;; The new version of make-register introduces:
;; - the 'trace-on' flag (initialised to false)
;; - printing the register name, its old and new contents
;;   if tracing is ON
;; - support for the trace-on and trace-off messages in
;;   its external interface

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when trace-on
                 (newline)
                 (display (list 'register-name '= name
                                'old-contents '= contents
                                'new-contents '= value)))
               (set! contents value)))
            ((eq? message 'trace-on) (set! trace-on true))
            ((eq? message 'trace-off) (set! trace-on false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))


;; Basic machine has been extended with the following procedures:

(define (reg-trace-on reg-name)
  ((lookup-register reg-name) 'trace-on))

(define (reg-trace-off reg-name)
  ((lookup-register reg-name) 'trace-off))

;; that have become part of its external interface (dispatch):

((eq? message 'reg-trace-on) reg-trace-on)
((eq? message 'reg-trace-off) reg-trace-off)


;; Test: exchange xyz values

(load "machine.scm")
(load "basic-machine-ext.scm")

(define xyz-machine
  (make-machine
   '(x y z)
   '()
   '((assign x (const 1))
     (assign y (const 2))
     (assign z (const 3))
     (save x)
     (save y)
     (save z)
     (restore x)
     (restore y)
     (restore z)
     done)))

;; Tracing is ON for x, y and z
((xyz-machine 'reg-trace-on) 'x)
((xyz-machine 'reg-trace-on) 'y)
((xyz-machine 'reg-trace-on) 'z)

(start xyz-machine)
;; (register-name = x old-contents = *unassigned* new-contents = 1)
;; (register-name = y old-contents = *unassigned* new-contents = 2)
;; (register-name = z old-contents = *unassigned* new-contents = 3)
;; (register-name = x old-contents = 1 new-contents = 3)
;; (register-name = y old-contents = 2 new-contents = 2)
;; (register-name = z old-contents = 3 new-contents = 1)
;; ;Value: done


;; Tracing is OFF for x, y and z
((xyz-machine 'reg-trace-off) 'x)
((xyz-machine 'reg-trace-off) 'y)
((xyz-machine 'reg-trace-off) 'z)

(start xyz-machine) ; done

(get-register-contents xyz-machine 'x) ; 3
(get-register-contents xyz-machine 'y) ; 2
(get-register-contents xyz-machine 'z) ; 1
