;; Exercise 4.60
;;
;; By giving the query

(lives-near ?person (Hacker Alyssa P))

;; Alyssa P. Hacker is able to find people who live near her,
;; with whom she can ride to work.
;;
;; On the other hand, when she tries to find all pairs of people
;; who live near each other by querying

(lives-near ?person-1 ?person-2)

;; she notices that each pair of people who live near each other
;; is listed twice; for example,

(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

;; Why does this happen?
;; Is there a way to find a list of people who live near each other,
;; in which each pair appears only once? Explain.


;; It happens due to the inability of the query system
;; to discern the direction of a relation:
;; two equal facts/relations presented independently.
;;
;; To avoid that, the rule can be provided with a way
;; to impose such a direction, which can be implemented
;; with the help of comparators, as shown below.


(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; Start with the initial rule.
;;
;; The following rule specifies that two people live near
;; each other if they live in the same town.
(assert!
 (rule (lives-near ?person-1 ?person-2)
       (and (address ?person-1 (?town . ?rest-1))
            (address ?person-2 (?town . ?rest-2))
            (not (same ?person-1 ?person-2)))))

(assert!
 (rule (same ?x ?x)))


(lives-near ?person (Hacker Alyssa P))
;; Query results:
;;
;; (lives-near (fect cy d) (hacker alyssa p))


(lives-near ?person-1 ?person-2)
;; Query results:
;;
;; (lives-near (aull dewitt) (reasoner louis))
;; (lives-near (aull dewitt) (bitdiddle ben))
;; (lives-near (reasoner louis) (aull dewitt))
;; (lives-near (reasoner louis) (bitdiddle ben))
;; (lives-near (hacker alyssa p) (fect cy d))
;; (lives-near (fect cy d) (hacker alyssa p))
;; (lives-near (bitdiddle ben) (aull dewitt))
;; (lives-near (bitdiddle ben) (reasoner louis))


;; Next, introduce a comparator that accepts Scheme values
;; and orders them in ASC order (DESC fits as well):

(define (<default . args)
  (apply <? (cons (make-default-comparator) args)))

;; (<default 1 2 3) ; true
;; (<default 1 2 1) ; false

;; (<default (list 1) (list 2) (list 3)) ; true
;; (<default (list 1) (list 2) (list 1)) ; false

;; (<default (list 1 2 3) (list 1 2 4)) ; true
;; (<default (list 1 2 3) (list 1 2 1)) ; false

;; See: https://srfi.schemers.org/srfi-128/srfi-128.html


;; Instruct the rule that we are only interested in
;; a specific subset of the relations: those where
;; person-1's name comes first (alphabetically).
;;
;; Optional: the part that checks the equality of person-1
;;           and person-2 is no longer needed and can be removed.
(assert!
 (rule (lives-near-each-other ?person-1 ?person-2)
       (and (address ?person-1 (?town . ?rest-1))
            (address ?person-2 (?town . ?rest-2))
            (lisp-value <default ?person-1 ?person-2))))


(lives-near-each-other ?person-1 ?person-2)

;; Query results:
;;
;; (lives-near-each-other (aull dewitt) (reasoner louis))
;; (lives-near-each-other (aull dewitt) (bitdiddle ben))
;; (lives-near-each-other (fect cy d) (hacker alyssa p))
;; (lives-near-each-other (bitdiddle ben) (reasoner louis))
