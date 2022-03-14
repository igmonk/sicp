;; 4.4 Logic Programming
;;
;; The ideas and technology of implementing interpreters
;; (sections `4.1`, `4.2` and `4.3`) are applied to discuss
;; an interpreter for a logic programming language.
;;
;; The language is called the 'query language', because
;; it is useful for retrieving information from data bases by
;; formulating 'queries', or questions, expressed in the language.

;; The query language in terms of the general framework:
;;
;; - primitive elements: queries
;; - means of combination: compound queries
;; - means of abstraction: rules

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)


;; Unify (?x a ?y) and (?y ?z a)
(unify-match '((? x) a (? y))
             '((? y) (? z) a)
             '())

;; (((? y) . a) ((? z) . a) ((? x) ? y))
;;
;; ?y -> a
;; ?z -> a
;; ?x -> ?y


;; Run the driver loop
(query-driver-loop)


;; (assert! (job (Bitdiddle Ben) (computer wizard)))
;; (assert! (rule (wheel ?person)
;;                (and (supervisor ?middle-manager ?person)
;;                     (supervisor ?x ?middle-manager))))

;; Simple queries
;;
;; The query languages's processing of simple queries
;; can be described as follows:
;; - the system finds all assignments to variables in the query pattern
;;   that satisfy the pattern - that is, all sets of values for the
;;   variables such that if the pattern variables are instantiated with
;;   (replaced by) the values, the result is in the data base
;; - the system responds to the query by listing all instantiations
;;   of the query pattern with the variable assignments that satisfy it

(job ?x (computer wizard))

;; Query results:
;;
;; (job (bitdiddle ben) (computer wizard))


;; Find all computer programmers
(job ?x (computer programmer))

;; Query results:
;;
;; (job (fect cy d) (computer programmer))
;; (job (hacker alyssa p) (computer programmer))


;; List all the employees' addresses
(address ?x ?y)

;; Query results:
;;
;; (address (aull dewitt) (slumerville (onion square) 5))
;; (address (cratchet robert) (allston (n harvard street) 16))
;; (address (scrooge eben) (weston (shady lane) 10))
;; (address (warbucks oliver) (swellesley (top heap road)))
;; (address (reasoner louis) (slumerville (pine tree road) 80))
;; (address (tweakit lem e) (boston (bay state road) 22))
;; (address (fect cy d) (cambridge (ames street) 3))
;; (address (hacker alyssa p) (cambridge (mass ave) 78))
;; (address (bitdiddle ben) (slumerville (ridge road) 10))


;; Find all people who supervise themselves
(supervisor ?x ?x)

;; Query results:
;;
;; None


;; Finds all job entries whose 3rd item is a 2-element list
;; whose first item is 'computer'.
(job ?x (computer ?type))

;; Query results:
;;
;; (job (tweakit lem e) (computer technician))
;; (job (fect cy d) (computer programmer))
;; (job (hacker alyssa p) (computer programmer))
;; (job (bitdiddle ben) (computer wizard))


;; Same except that the 3rd item could be any list
;; beginning with 'computer'.
(job ?x (computer . ?type))

;; Query results:
;;
;; (job (reasoner louis) (computer programmer trainee))
;; (job (tweakit lem e) (computer technician))
;; (job (fect cy d) (computer programmer))
;; (job (hacker alyssa p) (computer programmer))
;; (job (bitdiddle ben) (computer wizard))


;; Compound queries
;;
;; In order to form compound operations, the query language
;; provides means of combination: and, or and not.


;; Find the addresses of all the computer proogrammers
(and (job ?person (computer programmer))
     (address ?person ?where))

;; Query results:
;;
;; (and (job (fect cy d) (computer programmer))
;;      (address (fect cy d) (cambridge (ames street) 3)))
;; (and (job (hacker alyssa p) (computer programmer))
;;      (address (hacker alyssa p) (cambridge (mass ave) 78)))


;; Find all employees supervised by Ben Bitdiddle or Alyssa P. Hacker
(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; Query results:
;;
;; (or (supervisor (tweakit lem e) (bitdiddle ben))
;;     (supervisor (tweakit lem e) (hacker alyssa p)))
;; (or (supervisor (reasoner louis) (bitdiddle ben))
;;     (supervisor (reasoner louis) (hacker alyssa p)))
;; (or (supervisor (fect cy d) (bitdiddle ben))
;;     (supervisor (fect cy d) (hacker alyssa p)))
;; (or (supervisor (hacker alyssa p) (bitdiddle ben))
;;     (supervisor (hacker alyssa p) (hacker alyssa p)))


;; Find all people supervised by Ben Bitdiddle who are not
;; computer programmers.
(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

;; Query results:
;;
;; (and (supervisor (tweakit lem e) (bitdiddle ben))
;;      (not (job (tweakit lem e) (computer programmer))))


;; Find all people whose salary is greater than 30,000
(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

;; Query results:
;;
;; (and (salary (scrooge eben) 75000)
;;      (lisp-value > 75000 30000))
;; (and (salary (warbucks oliver) 150000)
;;      (lisp-value > 150000 30000))
;; (and (salary (fect cy d) 35000)
;;      (lisp-value > 35000 30000))
;; (and (salary (hacker alyssa p) 40000)
;;      (lisp-value > 40000 30000))
;; (and (salary (bitdiddle ben) 60000)
;;      (lisp-value > 60000 30000))


;; Rules
;;
;; Rules are the means for abstracting queries.


;; The following rule specifies that two people live near
;; each other if they live in the same town.
(assert!
 (rule (lives-near ?person-1 ?person-2)
       (and (address ?person-1 (?town . ?rest-1))
            (address ?person-2 (?town . ?rest-2))
            (not (same ?person-1 ?person-2)))))

(assert!
 (rule (same ?x ?x)))

;; The following rule declares that a person is a 'wheel'
;; in an organization if he supervises someone who is
;; in turn a supervisor:
(assert!
 (rule (wheel ?person)
       (and (supervisor ?middle-manager ?person)
            (supervisor ?x ?middle-manager))))


;; Find all people who live near Ben Bitdiddle
(lives-near ?x (Bitdiddle Ben))

;; Query results:
;;
;; (lives-near (aull dewitt) (bitdiddle ben))
;; (lives-near (reasoner louis) (bitdiddle ben))


;; Find all computer programmers who live near Ben Bitdiddle
(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))

;; Query results:
;;
;; None


;; Rules can be used as parts of other rules
;; or even be defined recursively.

;; The following rule says that a staff person is outranked
;; by a boss in the organization if the boss is the person's
;; supervisor or (recursively) if the person's supervisor
;; is outranked by the boss.
(assert!
 (rule (outranked-by ?staff-person ?boss)
       (or (supervisor ?staff-person ?boss)
           (and (supervisor ?staff-person ?middle-manager)
                (outranked-by ?middle-manager ?boss)))))


;; Find all employees outranked by Ben Bitdiddle
(outranked-by ?x (Bitdiddle Ben))

;; Query results:
;;
;; (outranked-by (tweakit lem e) (bitdiddle ben))
;; (outranked-by (reasoner louis) (bitdiddle ben))
;; (outranked-by (fect cy d) (bitdiddle ben))
;; (outranked-by (hacker alyssa p) (bitdiddle ben))


;; Logic as programs
;;
;; We can regard a rule as a kind of logical implication:
;; If an assignment of values to pattern variables satisfies the body,
;; then it satisfies the conclusion.
;;
;; Consequently, we can regard the query language as having the ability
;; to perform 'logical deductions' based upon the rules.

;; Append
;;
;; - for any list y, the empty list and y append to form y
;; - for any u, v, y, and z, (cons u v) and y append to form
;;   (cons u z) if v and y append to form z
;;
;; To express this in the query language, two rules are defined
;; for a relation (x and y append to form z):
;;
;; (append-to-form x y z)

(assert!
 (rule (append-to-form () ?y ?y)))

(assert!
 (rule (append-to-form (?u . ?v) ?y (?u . ?z))
       (append-to-form ?v ?y ?z)))

;; The 1st rule has no body, which means that the conclusion
;; holds for any value of ?y.
;; The 2nd rule makes use of dotted-tail notation to name
;; the car and cdr of a list.

;; Compute the append of two lists
(append-to-form (a b) (c d) ?z)

;; Query results:
;;
;; (append-to-form (a b) (c d) (a b c d))


;; Which list, when appended to (a b), yields (a b c d)?
(append-to-form (a b) ?y (a b c d))

;; Query results:
;;
;; (append-to-form (a b) (c d) (a b c d))


;; Find all pairs of lists that append to form (a b c d)
(append-to-form ?x ?y (a b c d))

;; Query results:
;;
;; (append-to-form (a b c d) () (a b c d))
;; (append-to-form () (a b c d) (a b c d))
;; (append-to-form (a) (b c d) (a b c d))
;; (append-to-form (a b) (c d) (a b c d))
;; (append-to-form (a b c) (d) (a b c d))



