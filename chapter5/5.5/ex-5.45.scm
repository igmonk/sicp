;; Exercise 5.45
;;
;; By comparing the stack operations used by compiled code to
;; the stack operations used by the evaluator for the same computation,
;; we can determine the extent to which the compiler optimizes use of
;; the stack, both in speed (reducing the total number of stack operations)
;; and in space (reducing the maximum stack depth).
;;
;; Comparing this optimized stack use to the performance of
;; a special-purpose machine for the same computation gives some indication
;; of the quality of the compiler.
;;
;; a. Exercise 5.27 asked you to determine, as a function of n,
;;    the number of pushes and the maximum stack depth needed by
;;    the evaluator to compute n! using the recursive factorial procedure
;;    given above.
;;
;;    Exercise 5.14 asked you to do the same measurements for
;;    the special-purpose factorial machine shown in figure 5.11.
;;
;;    Now perform the same analysis using the compiled factorial procedure.

(load "compiler-factory.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(load "compile-and-go.scm")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(factorial 1) ; (total-pushes = 7 maximum-depth = 3)
(factorial 2) ; (total-pushes = 13 maximum-depth = 5)
(factorial 3) ; (total-pushes = 19 maximum-depth = 8)
(factorial 4) ; (total-pushes = 25 maximum-depth = 11)
(factorial 5) ; (total-pushes = 31 maximum-depth = 14)

;; total-pushes
;;
;;       slope: (31 - 25) / (5 - 4) = 6
;; y-intercept: 6 * 5 + y0 = 31
;;              y0 = 1
;;
;; Line equation: y = 6x + 1

;; maximum-depth
;;
;;       slope: (14 - 11) / (5 - 4) = 3
;; y-intercept: 3 * 5 + y0 = 14
;;              y0 = -1
;;
;; Line equation: y = 3x - 1


;; Take the ratio of the number of pushes in the compiled version
;; to the number of pushes in the interpreted version, and do the same
;; for the maximum stack depth.
;;
;; Since the number of operations and the stack depth used to compute n!
;; are linear in n, these ratios should approach constants as n becomes
;; large. What are these constants?
;;
;; Similarly, find the ratios of the stack usage in the special-purpose
;; machine to the usage in the interpreted version.
;;
;; Compare the ratios for special-purpose versus interpreted code to
;; the ratios for compiled versus interpreted code.
;;
;; You should find that the special-purpose machine does much better
;; than the compiled code, since the hand-tailored controller code
;; should be much better than what is produced by our rudimentary
;; general-purpose compiler.


;; Recall the following functions of n from exercises 5.27 and 5.14:
;;
;; exercise 5.27 (interpreted)
;; - total pushes: 32n - 16
;; - maximum depth: 5n + 3
;;
;; exercise 5.14 (special purpose)
;; - total pushes: 2n - 2
;; - maximum depth: 2n - 2
;;
;; and build the comparison table as shown below.


;; -----------------------|--------------|---------------|
;;                        | total pushes | maximum depth |
;; -----------------------|--------------|---------------|
;; interpreted            |   32n - 16   |    5n + 3     |
;; special purpose (s.p.) |    2n - 2    |    2n - 2     |
;; compiled               |    6n + 1    |    3n - 1     |
;; -----------------------|--------------|---------------|
;; compiled / interpreted |     3/16     |      3/5      | as n -> Inf+
;; s.p. / interpreted     |     1/16     |      2/5      | as n -> Inf+
;; -----------------------|--------------|---------------|
;; s.p. / compiled        |     1/3      |      2/3      | as n -> Inf+
;; -----------------------|--------------|---------------|


;; As indicated by the table above, in comparison with the compiled code
;; the special purpose machine requires:
;; - 3 times less total pushes
;; - 1.5 times less maximum stack depth


;; b. Can you suggest improvements to the compiler that would help it
;;    generate code that would come closer in performance to
;;    the hand-tailored version?


;; The object code for the factorial procedure given above
;; contains a few places where a primitive procedure (=, *, -)
;; is about to be applied and some registers must be saved
;; onto the stack beforehand:

(save continue)
(save env)
(assign proc (op lookup-variable-value) (const =) (reg env))
<...>
(assign proc (op lookup-variable-value) (const *) (reg env))
<...>
(save proc)
(assign proc (op lookup-variable-value) (const -) (reg env))
<...>

;; In addition, the application of a primitive procedure (its
;; compiled code) contains two parallel branches - one for
;; primitive procedures and another for compound procedures -
;; and a test instruction to chose one of them:

(test (op primitive-procedure?) (reg proc)) ;; =
(branch (label primitive-branch17))
compiled-branch16
<...>
(test (op primitive-procedure?) (reg proc)) ;; -
(branch (label primitive-branch8))
compiled-branch7
<...>
(test (op primitive-procedure?) (reg proc)) ;; *
(branch (label primitive-branch14))
compiled-branch13


;; The hand-tailored version is devoid of such a flaw.
;; The compiled version can be improved as suggested in
;; exercise 5.38 - open-code primitives - generation of
;; code to directly use the primitive machine operations.
;;
;; For ex., expression (+ a 1) might be compiled into
;; something as simple as
;;
(assign val (op lookup-variable-value) (const a) (reg env))
(assign val (op +) (reg val) (const 1))
