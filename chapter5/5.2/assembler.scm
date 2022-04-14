;; The Assembler
;;
;; The assembler transforms the sequence of controller expressions
;; for a machine into a corresponding list of machine instructions,
;; each with its execution procedure.
;;
;; The assembler begins by scanning the controller text to separate
;; the labels from the instructions. As it scans the text,
;; it constructs both a list of instructions and a table that
;; associates each label with a pointer into that list.
;;
;; Then the assembler augments the instruction list by inserting
;; the execution procedure for each instruction.


(load "../../chapter3/3.3/table-obj-2d.scm")
(load "register.scm")
(load "instruction.scm")
(load "label.scm")
(load "constant.scm")
(load "operation.scm")

;; The assemble procedure takes the controller text and the machine
;; model as arguments and returns the instruction sequence to be
;; stored in the model.

(define (make-assembler machine)
  (let ((dt (make-table)))

    (define (assemble controller-text)
      (extract-labels
       controller-text
       (lambda (insts labels)
         (update-insts! insts labels)
         insts)))

    ;; extract-labels takes as arguments
    ;; - a list 'text' (the sequence of controller instruction expressions)
    ;; - a 'receive' procedure, that will be called with two values:
    ;;   1) a list 'insts' of instruction data structures
    ;;   2) a table which associates each label from 'text'
    ;;      with the position in the list 'insts' that the label designates
    (define (extract-labels text receive)
      (if (null? text)
          (receive '() '())
          (extract-labels
           (cdr text)
           (lambda (insts labels)
             (let ((next-inst (car text)))
               (if (symbol? next-inst)
                   (if (label-exists? labels next-inst)
                       (error "Label name already exists -- ASSEMBLE" next-inst)
                       (receive
                           insts
                           (cons (make-label-entry next-inst insts)
                                 labels)))
                   (receive
                       (cons (make-instruction next-inst) insts)
                       labels)))))))

    (define (label-exists? labels label-name)
      (assoc label-name labels))

    ;; update-insts! modifies the instruction list, which initially
    ;; contains only the text of the instructions, to include
    ;; the corresponding execution procedures:
    (define (update-insts! insts labels)
      (for-each
       (lambda (inst)
         (set-instruction-execution-proc!
          inst
          (make-execution-procedure
           (instruction-text inst) labels)))
       insts))

    ;; Execution Procedure
    ;;
    ;; For each type of instruction in the register-machine language,
    ;; there is a generator that builds an appropriate execution procedure.
    ;;
    ;; The details of these procedures determine both the syntax and meaning
    ;; of the individual instructions in the register-machine language.
    (define (make-execution-procedure inst labels)
      (let ((exec-proc (classify inst 'assemble)))
        (if exec-proc
            (exec-proc inst machine labels)
            (error "Unknown instruction type -- ASSEMBLE" inst))))

    (define (classify exp type)
      (if (pair? exp)
          (get type (car exp))
          false))

    ;; Operation Procedure
    ;;
    ;; The following procedure produces an execution procedure
    ;; for an 'operation expression' - a list containing
    ;; the operation and operand expressions from the instruction:
    (define (make-operation-proc exp labels)
      (let ((op (lookup-prim (operation-exp-op exp) (machine 'operations)))
            (aprocs
             (map (lambda (e)
                    (make-primitive-proc e labels))
                    ;; (if (label-exp? e) ; Causes compiled code to throw an error
                    ;;     (error "Operations can't be used with labels -- ASSEMBLE")
                    ;;     (make-primitive-proc e labels)))
                  (operation-exp-operands exp))))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs)))))

    ;; The following procedure generates execution procedures
    ;; to produce values for 'reg', 'label' and 'const' expressions
    ;; during the simulation:
    (define (make-primitive-proc exp labels)
      (cond ((constant-exp? exp)
             (let ((c (constant-exp-value exp)))
               (lambda () c)))
            ((label-exp? exp)
             (let ((insts
                    (lookup-label labels
                                  (label-exp-label exp))))
               (lambda () insts)))
            ((register-exp? exp)
             (let ((reg ((machine 'get-register)
                         (register-exp-reg exp))))
               (lambda () (get-contents reg))))
            (else
             (error "Unknown expression type -- ASSEMBLE" exp))))

    ;; Lookup the operation name in the operation table
    ;; for the machine:
    (define (lookup-prim symbol operations)
      (let ((val (assoc symbol operations)))
        (if val
            (cadr val)
            (error "Unknown operation -- ASSEMBLE" symbol))))

    ;; If the label with the given name is found,
    ;; lookup-label returns the sequence of instructions
    ;; the label points to.
    (define (lookup-label labels label-name)
      (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "Undefined label -- ASSEMBLE" label-name))))

    ;; Extensions support
    (define (extend-assemble type proc)
      (put 'assemble type proc))
    
    ;; Dispatch table procedures
    (define get (dt 'lookup-proc))
    (define put (dt 'insert-proc!))

    ;; Interface to the rest of the system
    (define (dispatch m)
      (cond ((eq? m 'assemble) assemble)
            ((eq? m 'extend-assemble) extend-assemble)
            ((eq? m 'make-operation-proc) make-operation-proc)
            ((eq? m 'make-primitive-proc) make-primitive-proc)
            ((eq? m 'lookup-label) lookup-label)
            (else
             (error "Unknown operation -- MAKE-ASSEMBLER" m))))

    dispatch))
