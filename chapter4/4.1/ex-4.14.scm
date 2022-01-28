;; Exercise 4.14
;;
;; Eva Lu Ator and Louis Reasoner are each experimenting with
;; the metacircular evaluator.
;;
;; Eva types in the definition of map, and runs some test programs
;; that use it. They work fine.
;;
;; Louis, in contrast, has installed the system version of map
;; as a primitive for the metacircular evaluator.
;; When he tries it, things go terribly wrong.
;;
;; Explain why Louis's map fails even though Eva's works.


;; The object of the system version of 'map' installed as
;; a primitive procedure relies on the procedure 'apply',
;; which is part of the implementation language
;; (the one the evaluator is written in) and
;; whose procedure application method is different from
;; that pertaining to the implemented language.
;;
;; Thus, when given either primitive or compound procedure
;; in the form of a list whose car is 'primitive or 'procedure,
;; respectively, the procedure 'apply' is not able to do its job.


;; Prerequisite
;;
;; Install the procedure 'map' as a primitive procedure:
;;
;; (define primitive-procedures
;;   (list (list 'map map)
;;         ;; <more primitives>
;;         ))


;; a. Primitive procedure
;;
;; (eval-exp '(map not '(list true true true)))
;;
;; The object (primitive #[compiled-procedure 157 ...)
;; is not applicable.


;; b. Compound procedure
;;
;; (eval-exp '(map (lambda (x) (* x 2)) '(1 2 3 4 5)))
;;
;; The object (procedure (x) ((* x 2)) #[compound-procedure ...)
;; is not applicable.
