;; Exercise 5.49
;;
;; As an alternative to using the explicit-control evaluator's
;; read-eval-print loop, design a register machine that performs
;; a read-compile-execute-print loop.
;;
;; That is, the machine should run a loop that reads an expression,
;; compiles it, assembles and executes the resulting code, and
;; prints the result.
;;
;; This is easy to run in our simulated setup, since we can
;; arrange to call the procedures compile and assemble as
;; "register-machine operations".


;; Start with the implementation of a new register machine
;; operation named compile-and-assemble:

(define compile ((create-compiler) 'compile))

(define (compile-and-assemble exp)
  (let ((assemble ((rcepl-machine 'assembler) 'assemble)))
    (assemble
     (statements (compile exp 'val 'return)))))


;; Next, clone the explicit-control machine and extend its
;; operations:

(define rcepl-machine
  (make-machine
   '(exp env val continue proc argl unev compapp printres)
   (cons (list 'compile-and-assemble compile-and-assemble)
         eceval-operations)
   <...>
   ))


;; Next, adjust the instructions as indicated below:

read-compile-execute-print-loop ;; Instead of read-eval-print-loop
(perform (op initialize-stack))
(perform (op prompt-for-input)
         (const ";;; EC-Eval input:"))
(assign exp (op read))
(assign val (op compile-and-assemble) (reg exp)) ;; Invoke compile-and-assemble
(assign env (op get-global-environment))
(assign continue (label print-result))
(goto (reg val)) ;; Branch to what's been saved to val by compile-and-assemble

print-result
(perform (op print-stack-statistics))
(perform (op announce-output)
         (const ";;; EC-Eval value:"))
(perform (op user-print) (reg val))
(goto (label read-compile-execute-print-loop)) ;; Instead of read-eval-print-loop

<...>

signal-error
(perform (op user-print) (reg val))
(goto (label read-compile-execute-print-loop)) ;; Instead of read-eval-print-loop



;; Tests

(load "rcepl-machine.scm")

;; Set flag to false and start the machine:
(set-register-contents! rcepl-machine 'flag false)
(start rcepl-machine)


;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 26 maximum-depth = 14)
;;; EC-Eval value:
120

;;; EC-Eval input:
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 10)

(total-pushes = 882 maximum-depth = 29)
;;; EC-Eval value:
55

;;; EC-Eval input:
