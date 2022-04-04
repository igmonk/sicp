;; 5.2 A Register-Machine Simulator
;;
;; In this section we construct a simulator for machines
;; described in the register-machine language.
;;
;; The simulator is a Scheme program with four interface procedures:
;;
;; - (make-machine <register-names> <operations> <controller>)
;;
;;   constructs and returns a model of the machine with the given registers,
;;   operations, and controller.
;;
;; - (set-register-contents! <machine-model> <register-name> <value>)
;;
;;   stores a value in a simulated register in the given machine.
;;
;; - (get-register-contents <machine-model> <register-name>)
;;
;;   returns the contents of a simulated register in the given machine.
;;
;; - (start <machine-model>)
;;
;;   simulates the execution of the given machine, starting from
;;   the beginning of the controller sequence and stopping when
;;   it reaches the end of the sequence.

(load "machine.scm")
(load "basic-machine-ext.scm")

;; As an example of how these procedures are used, one can define
;; gcd-machine to be a model of the GCD machine of section 5.1.1
;; as follows:

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

;; To compute GCDs with this machine, we
;; - set the input registers,
;; - start the machine, and
;; - examine the result when the simulation terminates:

(set-register-contents! gcd-machine 'a 206) ; done
(set-register-contents! gcd-machine 'b 40)  ; done
(start gcd-machine)                         ; done
(get-register-contents gcd-machine 'a)      ; 2
(get-register-contents gcd-machine 'b)      ; 0

