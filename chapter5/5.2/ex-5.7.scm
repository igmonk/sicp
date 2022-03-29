;; Exercise 5.7
;;
;; Use the simulator to test the machines you designed
;; in exercise 5.4.

(load "machine.scm")
(load "basic-machine-ext.scm")

;; a. Recursive exponentiation:

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

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

;; (set-register-contents! expt-rec-machine 'b 2) ; done
;; (set-register-contents! expt-rec-machine 'n 8) ; done
;; (start expt-rec-machine)                       ; done
;; (get-register-contents expt-rec-machine 'val)  ; 256


;; b. Iterative exponentiation:

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(define expt-iter-machine
  (make-machine
   '(b n counter product)
   (list (list '= =) (list '- -) (list '* *))
   '((assign counter (reg n))
     (assign product (const 1))
     expt-loop
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-loop))
     expt-done)))

;; (set-register-contents! expt-iter-machine 'b 2)    ; done
;; (set-register-contents! expt-iter-machine 'n 8)    ; done
;; (start expt-iter-machine)                          ; done
;; (get-register-contents expt-iter-machine 'product) ; 256
