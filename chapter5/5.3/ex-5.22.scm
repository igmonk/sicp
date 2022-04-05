;; Exercise 5.22
;;
;; Exercise 3.12 of section 3.3.1 presented an append procedure
;; that appends two lists to form a new list and an append! procedure
;; that splices two lists together.
;;
;; Design a register machine to implement each of these procedures.
;; Assume that the list-structure memory operations are available as
;; primitive operations.


;; cd to sicp/chapter5/5.2

(load "machine.scm")
(load "basic-machine-ext.scm")


;; 1. append

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
   '(x y val continue)
   (list (list 'null? null?) (list 'cons cons)
         (list 'car car) (list 'cdr cdr))
   '((assign continue (label done))
     loop
     (test (op null?) (reg x))
     (branch (label base-case))
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label after-append-cdr-x))
     (goto (label loop))
     after-append-cdr-x
     (restore x)
     (restore continue)
     (assign x (op car) (reg x))
     (assign val (op cons) (reg x) (reg val))
     (goto (reg continue))
     base-case
     (assign val (reg y))
     (goto (reg continue))
     done)))


;; Tests

(set-register-contents! append-machine 'x '()) ; done
(set-register-contents! append-machine 'y '()) ; done
(start append-machine)                         ; done
(get-register-contents append-machine 'val)    ; ()

(set-register-contents! append-machine 'x '(1 2 3)) ; done
(set-register-contents! append-machine 'y '())      ; done
(start append-machine)                              ; done
(get-register-contents append-machine 'val)         ; (1 2 3)

(set-register-contents! append-machine 'x '())      ; done
(set-register-contents! append-machine 'y '(4 5 6)) ; done
(start append-machine)                              ; done
(get-register-contents append-machine 'val)         ; (4 5 6)

(set-register-contents! append-machine 'x '(1 2 3)) ; done
(set-register-contents! append-machine 'y '(4 5 6)) ; done
(start append-machine)                              ; done
(get-register-contents append-machine 'val)         ; (1 2 3 4 5 6)


;; 2. append!

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define append!-machine
  (make-machine
   '(x y lp tmp)
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'set-cdr! set-cdr!))
   '((assign lp (reg x))
     last-pair-loop
     (assign tmp (op cdr) (reg lp))
     (test (op null?) (reg tmp))
     (branch (label last-pair-done))
     (assign lp (reg tmp))          ; OR: (assign lp (op cdr) (reg lp))
     (goto (label last-pair-loop))
     last-pair-done
     (perform (op set-cdr!) (reg lp) (reg y)))))


;; Tests

(set-register-contents! append!-machine 'x '(1 2 3)) ; done
(set-register-contents! append!-machine 'y '())      ; done
(start append!-machine)                              ; done
(get-register-contents append!-machine 'x)           ; (1 2 3)

(set-register-contents! append!-machine 'x '(1 2 3)) ; done
(set-register-contents! append!-machine 'y '(4 5 6)) ; done
(start append!-machine)                              ; done
(get-register-contents append!-machine 'x)           ; (1 2 3 4 5 6)
