;; Exercise 4.57
;;
;; Define a rule that says that person 1 can replace person 2
;; if either person 1 does the same job as person 2 or
;; someone who does person 1's job can also do person 2's job,
;; and if person 1 and person 2 are not the same person.
;;
;; Using your rule, give queries that find the following:
;;
;; a. all people who can replace Cy D. Fect
;; b. all people who can replace someone who is being paid more
;;    than they are, together with the two salaries

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; Import the 'same' rule
(assert!
 (rule (same ?x ?x)))


;; Below are the two alternative 'can-replace' rule definitions:
;;
;; 1. Work with the two 'or' parts of the rule body separately:
;;    first, check if the positions are the same, while ensuring
;;    the persons are different; and then, check if one can do
;;    the job of another, making sure they are different people.
;; 2. Label the job occupations differently and apply filtering
;;    based on the 'or' alternatives after having ensured
;;    the persons are different.


;; 1. Head-on solution
(assert!
 (rule (can-replace ?person-1 ?person-2)
       (or (and (job ?person-1 ?position)
                (job ?person-2 ?position)
                (not (same ?person-1 ?person-2)))
           (and (job ?person-1 ?position-1)
                (job ?person-2 ?position-2)
                (can-do-job ?position-1 ?position-2)
                (not (same ?person-1 ?person-2))))))


;; 2. A more sophisticated one
(assert!
 (rule (can-replace ?person-1 ?person-2)
       (and (job ?person-1 ?position-1)
            (job ?person-2 ?position-2)
            (not (same ?person-1 ?person-2))
            (or (lisp-value equal? ?position-1 ?position-2)
                (can-do-job ?position-1 ?position-2)))))


;; a. all people who can replace Cy D. Fect

(can-replace ?x (Fect Cy D))

;; Query results:
;;
;; (can-replace (hacker alyssa p) (fect cy d))
;; (can-replace (bitdiddle ben) (fect cy d))


;; b. all people who can replace someone who is being paid more
;;    than they are, together with the two salaries

(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (lisp-value > ?salary-1 ?salary-2))

;; Query results:
;;
;; (and (can-replace (hacker alyssa p) (fect cy d))
;;      (salary (hacker alyssa p) 40000)
;;      (salary (fect cy d) 35000)
;;      (lisp-value > 40000 35000))
;; (and (can-replace (fect cy d) (reasoner louis))
;;      (salary (fect cy d) 35000)
;;      (salary (reasoner louis) 30000)
;;      (lisp-value > 35000 30000))
;; (and (can-replace (hacker alyssa p) (reasoner louis))
;;      (salary (hacker alyssa p) 40000)
;;      (salary (reasoner louis) 30000)
;;      (lisp-value > 40000 30000))
;; (and (can-replace (bitdiddle ben) (tweakit lem e))
;;      (salary (bitdiddle ben) 60000)
;;      (salary (tweakit lem e) 25000)
;;      (lisp-value > 60000 25000))
;; (and (can-replace (bitdiddle ben) (fect cy d))
;;      (salary (bitdiddle ben) 60000)
;;      (salary (fect cy d) 35000)
;;      (lisp-value > 60000 35000))
;; (and (can-replace (bitdiddle ben) (hacker alyssa p))
;;      (salary (bitdiddle ben) 60000)
;;      (salary (hacker alyssa p) 40000)
;;      (lisp-value > 60000 40000))
