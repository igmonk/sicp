;; Exercise 5.33
;;
;; Consider the following definition of a factorial procedure,
;; which is slightly different from the one given above:

(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))


(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-factorial-alt
  (compile
   '(define (factorial-alt n)
      (if (= n 1)
          1
          (* n (factorial-alt (- n 1)))))
   'val
   'next))

(instruction-seq->file compiled-factorial-alt "factorial-alt.obj")


;; The source code of the two factorial implementations differ
;; in the way the recursive call is made, namely, the order of
;; the arguments to the procedure is reverse.
;;
;; Due to the arguments evaluation order in the compiler
;; (see the construct-arglist entry point), the argument that
;; comes last in a combination gets compiled first.
;; Thus, the order in which the arguments are compiled is
;; right to left.
;;
;; The optimizations applied by the compiler are the following:
;;
;; - argl must be preserved around each operand evaluation except
;;   the first (so that arguments accumulated so far won't be lost)
;; - env must be preserved around each operand evaluation except
;;   the last (for use by subsequent operand evaluations).
;;
;; This can be seen in the object code:

false-branch4
(assign proc (op lookup-variable-value) (const *) (reg env))
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const -) (reg env))
(assign val (const 1))

;; As shown above, the register env is saved before the procedure
;; factorial-alt gets invoked, since it can change the environment.
;;
;; In contrast, the original factorial implementation is spared of
;; this stack operation:

false-branch4
(assign proc (op lookup-variable-value) (const *) (reg env))
(save continue)
(save proc)
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op list) (reg val))
(save argl)
(assign proc (op lookup-variable-value) (const factorial) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const -) (reg env))
(assign val (const 1))


;; On the other hand, the situation is opposite when it comes to
;; the register argl.
;;
;; The alternative factorial implementation is free from saving
;; and restoring argl around the evaluation of the first operand:

after-call9
(assign argl (op list) (reg val))
(restore env)
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))

;; As shown above, there is no need to restore argl, since it was
;; not necessary to save it beforehand (around the first operand
;; evaluation).
;;
;; The original factorial implementation, however, must restore
;; the register argl what was saved before (see false-branch4 of
;; the object code for the original implementation above):

after-call9
(restore argl)
(assign argl (op cons) (reg val) (reg argl))


;; In total, the total number of instructions is exactly the same
;; for both factorial implementations.
;;
;; Each recursive call, the original one saves and restores
;; the register argl, whereas the alternative one saves and
;; restores the register env.
;;
;; There is a subtle difference in their impact on performance,
;; where only the bare minimum of information is saved onto
;; the stack: the list of pending functions and their arguments
;; (instead of saving and restoring whole entironments).
;;
;; The optimization of treating the last operand specially
;; is known as evlis tail recursion:
;; https://www.akalin.com/evlis-tail-recursion
;;
;; Proper tail recursion:
;; https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-6.html#%25_sec_3.5
