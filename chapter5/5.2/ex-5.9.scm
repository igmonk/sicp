;; Exercise 5.9
;;
;; The treatment of machine operations above permits them
;; to operate on labels as well as on constants and
;; the contents of registers.
;;
;; Modify the expression-processing procedures to enforce
;; the condition that operations can be used only with
;; registers and constants.


;; The desired behaviour can be achieved by installing
;; a check into operation execution procedures that
;; throws an error if it is given a label:

(define (make-operation-proc exp labels)
  (let ((op (lookup-prim (operation-exp-op exp) (machine 'operations)))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Operations can't be used with labels -- ASSEMBLE")
                    (make-primitive-proc e labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; See: assembler.scm


;; Test

(load "machine.scm")
(load "basic-machine-ext.scm")

;; The following machine creation throws an error:

(define op-with-label-machine
  (make-machine
   '()
   (list (list '= =))
   '(start
     (test (op =) (label start) (label end))
     (branch (label start))
     end)))

;; Operations can't be used with labels -- ASSEMBLE
