;; Exercise 5.36
;;
;; What order of evaluation does our compiler produce for
;; operands of a combination? Is it left-to-right, right-to-left,
;; or some other order?
;;
;; Where in the compiler is this order determined?
;;
;; Modify the compiler so that it produces some other order
;; of evaluation. (See the discussion of order of evaluation for
;; the explicit-control evaluator in section 5.4.1.)
;;
;; How does changing the order of operand evaluation affect
;; the efficiency of the code that constructs the argument list?


;; The compiler produces right-to-left order of evaluation for
;; operands of a combination.


;; The order is determined in the procedure construct-arglist,
;; which takes the instruction sequences for the operands
;; and combines them with code that constructs the list of
;; arguments in argl:
;;
;; <compilation of last operand, targeted to val>
;; (assign argl (op list) (reg val))
;; <compilation of next operand, targeted to val>
;; (assign argl (op cons) (reg val) (reg argl))
;; ...<compilation of first operand, targeted to val>
;; (assign argl (op cons) (reg val) (reg argl))
;;
;; Since the procedure cons the arguments onto argl in sequence,
;; it must start with the last argument and end with the first,
;; so that the arguments will appear in order from first to last
;; in the resulting list.
;;
;; The following optimisation is applied:
;;
;; - argl must be preserved around each operand evaluation except
;;   the first (so that arguments accumulated so far won't be lost)
;; - env must be preserved around each operand evaluation except
;;   the last (for use by subsequent operand evaluations).
;;
;; In order to process the arguments from last to first,
;; the procedure reverses the list of operand code sequences
;; from the order supplied by the caller procedure.


(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-f
  (compile
   '(f 1 2 3)
   'val
   'next))

(instruction-seq->file compiled-f "5.36.f.obj")


;; Original right-to-left order

(assign proc (op lookup-variable-value) (const f) (reg env))
(assign val (const 3))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 1))
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


;; Left-to-right order
;;
;; To inverse the arguments evaluation order, the following
;; changes must be applied to the procedures construct-arglist
;; and code-to-get-rest-args:

(define (construct-arglist operand-codes)
  (let ((operand-code operand-codes)) ;; Do not reverse
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl)
         `((assign argl (const()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 `((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      `((assign argl
                                (op append-el) (reg argl) (reg val))))))) ;; append-el
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;; Notice the newly added machine operation: append-el that
;; appends an element to a list:
(define (append-el l el)
  (append l (list el)))

;; And install it into the machine operations:
(list 'append-el append-el)

;; The object code produced by the compiler is given below:

(assign proc (op lookup-variable-value) (const f) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign argl (op append-el) (reg argl) (reg val))
(assign val (const 3))
(assign argl (op append-el) (reg argl) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1

;; Notice the order in which the arguments are evaluated.


;; Alternatively, the same effect could have been achieved
;; without setting up the operation append-el.
;; Instead, the register val could've been reused to make up
;; a one-element list with the content of val:

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      `((assign val (op list) (reg val)) ;; Prepare val for append
                        (assign argl
                                (op append) (reg argl) (reg val))))))) ;; append
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;; Thus, the object code contains an additional instruction
;; each time an argument gets evaluated:

(assign proc (op lookup-variable-value) (const f) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign val (op list) (reg val)) ;; Prepare val for append
(assign argl (op append) (reg argl) (reg val))
(assign val (const 3))
(assign val (op list) (reg val)) ;; Prepare val for append
(assign argl (op append) (reg argl) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1


;; Yet another alternative is to reverse the argument list
;; during runtime instead of doing it during compile time:

(define (construct-arglist operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl)
         `((assign argl (const()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 `((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (tack-on-instruction-sequence
               (preserving '(env)
                           code-to-get-last-arg
                           (code-to-get-rest-args
                            (cdr operand-codes)))
               (make-instruction-sequence
                '(argl) '(argl) ;; These have no impact
                `((assign argl (op reverse) (reg argl))))))))))

;; Add reverse to the machine operations:
(list 'reverse reverse)

;; The object code comes with one more instruction (reverse)
;; in comparison to the original way of evaluating arguments:

(assign proc (op lookup-variable-value) (const f) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 3))
(assign argl (op cons) (reg val) (reg argl))
(assign argl (op reverse) (reg argl)) ;; reverse
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
