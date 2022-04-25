;; Exercise 5.37
;;
;; One way to understand the compiler's preserving mechanism
;; for optimizing stack usage is to see what extra operations
;; would be generated if we did not use this idea.
;;
;; Modify preserving so that it always generates the save and
;; restore operations.
;;
;; Compile some simple expressions and identify the unnecessary
;; stack operations that are generated. Compare the code to that
;; generated with the preserving mechanism intact.


;; The procedure preserving appends the sequences in such a way
;; that the contents of each register in the set is preserved
;; over the execution of the first sequence, if this is needed
;; for the execution of the second sequence.
;;
;; To make is always generate the save and restore operations,
;; the check for register modification and necessity must be
;; dropped:

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving
         (cdr regs)
         (make-instruction-sequence
          (list-union (list first-reg)
                      (registers-needed seq1))
          (list-difference (registers-modified seq1)
                           (list first-reg))
          (append `((save ,first-reg))
                  (statements seq1)
                  `((restore ,first-reg))))
         seq2))))


;; As in exercise 5.31, the following source code might be
;; compiled to identify unnecessary stack operations that
;; get optimized by the original implementation:
;;
;; (f 'x 'y)

(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-1
  (compile
   '(f 'x 'y)
   'val
   'next))


;; Compiled code (unoptimized)
;;
;; All the stack operations are superfluous, since none of
;; the operations involved (looking up variable value, quote,
;; procedure application) require preserving a register that
;; is modified by the preceding operation.
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const f) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save env)
(save continue)
(assign val (const y))
(restore continue)
(assign argl (op list) (reg val))
(restore env)
(save argl)
(save continue)
(assign val (const x))
(restore continue)
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)
after-call1


;; Compiled code (optimized)

(assign proc (op lookup-variable-value) (const f) (reg env))
(assign val (const y))
(assign argl (op list) (reg val))
(assign val (const x))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
