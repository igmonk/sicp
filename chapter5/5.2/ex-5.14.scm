;; Exercise 5.14
;;
;; Measure the number of pushes and the maximum stack depth
;; required to compute n! for various small values of n
;; using the factorial machine shown in figure 5.11
;;
;; From your data determine formulas in terms of n for
;; the total number of push operations and the maximum
;; stack depth used in computing n! for any n > 1.
;;
;; Note that each of these is a linear function of n
;; and is thus determined by two constants.
;;
;; In order to get the statistics printed, you will have to
;; augment the factorial machine with instructions to
;; initialize the stack and print the statistics.
;;
;; You may want to also modify the machine so that it
;; repeatedly reads a value for n, computes the factorial,
;; and prints the result (as we did for the GCD machine
;; in figure 5.4), so that you will not have to repeatedly
;; invoke get-register-contents, set-register-contents!, and
;; start.

(load "machine.scm")
(load "basic-machine-ext.scm")

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =) (list '- -) (list '* *)
         (list 'read read) (list 'print print))
   '(fact-init
     (assign continue (label fact-done))     ; set up final return address
     (perform (op initialize-stack))
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
     (perform (op print) (const "Stack statistics:"))
     (perform (op print-stack-statistics))
     (goto (label fact-init)))))

(define (print . items)
  (newline)
  (for-each (lambda (item)
              (display item))
            items))


;; Tests

(start fact-machine)

;; Enter n:

1
;; n! = 1
;; total-pushes = 0
;; maximum-depth = 0

2
;; n! = 2
;; total-pushes = 2
;; maximum-depth = 2

3
;; n! = 6
;; total-pushes = 4
;; maximum-depth = 4

4
;; n! = 24
;; total-pushes = 6
;; maximum-depth = 6

5
;; n! = 120
;; total-pushes = 8
;; maximum-depth = 8

6
;; n! = 720
;; total-pushes = 10
;; maximum-depth = 10

7
;; n! = 5040
;; total-pushes = 12
;; maximum-depth = 12

8
;; n! = 40320
;; total-pushes = 14
;; maximum-depth = 14

9
;; n! = 362880
;; total-pushes = 16
;; maximum-depth = 16

10
;; n! = 3628800
;; total-pushes = 18
;; maximum-depth = 18


;; Based on the observations above,
;; the total number of push operations and the maximum stack depth
;; used in computing n! for any n > 1 can be calculated using the
;; following formula:
;;
;; (n - 1) * 2
;;
;; The original version of stack is used - a single stack instance
;; is used for saving and restoring values of all machine registers.
;;
;; The total number of push operations equals the maximum stack
;; depth since the recursion process is plain:
;; the stack grows monotonously until the process reaches
;; the base case, and just as monotonously does it shink back.
;;
;; No double (triple, etc.) recursion is involved.
;;
;; The factor of 2 in the formula above is explained by the fact
;; we push 2 items into the stack at each recursion step.
