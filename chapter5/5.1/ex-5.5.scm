;; Exercise 5.5
;;
;; Hand-simulate the factorial and Fibonacci machines, using some
;; nontrivial input (requiring execution of at least one recursive call).
;;
;; Show the contents of the stack at each significant point
;; in the execution.


;; Factorial

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))


;; Consider (factorial 3)

;;    stack: <empty>
;;        n: 3
;; continue: fact-done

;;    stack: (label fact-done) 3
;;        n: 2
;; continue: (label after-fact)

;;    stack: (label fact-done) 3 (label after-fact) 2
;;        n: 1
;; continue: (label after-fact)

;; n = 1 => (branch (label base-case))

;; [base-case]
;; val: 1

;; (goto (reg continue)) = (goto (label after-fact))

;;        n: 2
;; continue: (label after-fact)
;;    stack: (label fact-done) 3
;;      val: 2

;; (goto (reg continue)) = (goto (label after-fact))

;;        n: 3
;; continue: (label fact-done)
;;    stack: <empty>
;;      val: 6

;; (goto (reg continue)) = (goto (label fact-done))

;; Return value: 6
