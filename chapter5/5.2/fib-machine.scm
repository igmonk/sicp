;; Fibonacci Machine

(load "machine.scm")
(load "basic-machine-ext.scm")

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '+ +) (list '- -)
         (list 'read read) (list 'print print))
   '(fib-init
     (assign continue (label fib-done)) ; set up final return address
     (perform (op initialize-stack))
     (perform (op print) (const "Enter n: "))
     (assign n (op read))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fib-n-1))
     (goto (label fib-loop))
     after-fib-n-1 ; upon return, val contains Fib(n-1)
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label after-fib-n-2))
     (save val) ; save Fib(n-1)
     (goto (label fib-loop))
     after-fib-n-2 ; upon return, val contains Fib(n-2)
     (restore n) ; restore Fib(n-1)
     (restore continue)
     (assign val (op +) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (reg n))
     (goto (reg continue))
     fib-done
     (perform (op print) (const "Fib(n) = ") (reg val))
     (perform (op print) (const "Stack statistics:"))
     (perform (op print-stack-statistics))
     (goto (label fib-init)))))

(define (print . items)
  (newline)
  (for-each (lambda (item)
              (display item))
            items))


(start fib-machine)

;; Enter n:

2 ; (total-pushes = 3 maximum-depth = 2)
3 ; (total-pushes = 6 maximum-depth = 4)
4 ; (total-pushes = 12 maximum-depth = 6)
5 ; (total-pushes = 21 maximum-depth = 8)
6 ; (total-pushes = 36 maximum-depth = 10)
7 ; (total-pushes = 60 maximum-depth = 12)
8 ; (total-pushes = 99 maximum-depth = 14)
9 ; (total-pushes = 162 maximum-depth = 16)

;; total-pushes
;;
;; S(n) = a * Fib(n+1) + b
;;
;; S(4) = a * Fib(5) + b
;; S(5) = a * Fib(6) + b
;;
;; 12 = a * 5 + b
;; 21 = a * 8 + b
;;
;;  a = 3
;;  b = -3
;;
;; => S(n) = 3 * Fib(n+1) - 3

;; maximum-depth
;;
;; 2n - 2
