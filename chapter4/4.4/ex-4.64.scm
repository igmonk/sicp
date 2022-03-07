;; Exercise 4.64
;;
;; Louis Reasoner mistakenly deletes the outranked-by rule
;; (section 4.4.1) from the data base.
;;
;; When he realizes this, he quickly reinstalls it.
;; Unfortunately, he makes a slight change in the rule,
;; and types it in as

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

;; Just after Louis types this information into the system,
;; DeWitt Aull comes by to find out who outranks Ben Bitdiddle.
;;
;; He issues the query

(outranked-by (Bitdiddle Ben) ?who)

;; After answering, the system goes into an infinite loop.
;; Explain why.


;; Each time the recursive 'outranked-by' query is executed,
;; ?middle-manager stays unbound, causing the query system
;; repeatedly run the unification process.
;;
;; In contrast, in the original version of the rule, the
;; recursive search for the 'outranked by' is placed in
;; the second position of the 'and', where both
;; ?middle-manager and ?boss are bound to some values.
