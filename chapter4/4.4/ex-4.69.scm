;; Exercise 4.69
;;
;; Beginning with the data base and the rules you formulated
;; in exercise 4.63, devise a rule for adding 'greats' to
;; a grandson relationship.
;;
;; This should enable the system to deduce that Irad is
;; the great-grandson of Adam, or that Jabal and Jubal are
;; the great-great-great-great-great-grandsons of Adam.
;;
;; (Hint: Represent the fact about Irad, for example, as
;;
;;   ((great grandson) Adam Irad)
;;
;;   Write rules that determine if a list ends in the word 'grandson'.
;;
;;   Use this to express a rule that allows one to derive the relationship
;;
;;   ((great . ?rel) ?x ?y)
;;
;;   where ?rel is a list ending in grandson.)
;;
;; Check your rules on queries such as
;;
;; ((great grandson) ?g ?ggs) and (?relationship Adam Irad)

(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; Install the assertions from ex. 4.63
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))


;; Install the rules from ex. 4.63
;;
;; Grandson
(assert!
 (rule (grandson ?g ?s)
       (and (son ?f ?s)
            (son ?g ?f))))

;; An additional 'son' constituent from ex. 4.63
(assert!
 (rule (son ?m ?s)
       (and (wife ?m ?w)
            (son ?w ?s))))


;; Determine if a list ends in the word 'grandson'

(assert!
 (rule (ends-with-grandson (grandson))))

(assert!
 (rule (ends-with-grandson (?first . ?rest))
       (ends-with-grandson ?rest)))

;; (ends-with-grandson ())
;; (ends-with-grandson (one))
;; (ends-with-grandson (grandson))       ; (ends-with-grandson (grandson))
;; (ends-with-grandson (great grandson)) ; (ends-with-grandson (great grandson))
;; (ends-with-grandson (a b grandson))   ; (ends-with-grandson (a b grandson))


;; Define one more constituent of the 'grandson' rule
;; to account for the case with the 1-element list (grandson)
(assert!
 (rule ((grandson) ?g ?s)
       (grandson ?g ?s)))

;; Find all the grandson relationships, same as (grandson ?g ?s)
((grandson) ?g ?s)

;; Query results
;;
;; ((grandson) mehujael lamech)
;; ((grandson) irad methushael)
;; ((grandson) enoch mehujael)
;; ((grandson) cain irad)
;; ((grandson) adam enoch)
;; ((grandson) methushael jubal)
;; ((grandson) methushael jabal)
;; ((grandson) methushael jabal)


;; Define the 'great' rule
(assert!
 (rule ((great . ?rel) ?x ?y)
       (and (son ?x ?z)
            (?rel ?z ?y)
            (ends-with-grandson ?rel))))

;; Notice the 'ends-with-grandson' check is placed at the end,
;; as well as the check for ?rel is placed after ?z is bound
;; to some value by performing the search for sons.
;;
;; This is done so to avoid any recursive search before any of
;; the variables is bound to some value. Otherwise, a query
;; can result in the infinite loop.


;; Tests
;;
;; Find the great grandson relationships
((great grandson) ?g ?ggs)

;; Query results:
;;
;; ((great grandson) mehujael jubal)
;; ((great grandson) irad lamech)
;; ((great grandson) mehujael jabal)
;; ((great grandson) enoch methushael)
;; ((great grandson) cain mehujael)
;; ((great grandson) adam irad)


;; Find the relationship between Adam and Irad
(?relationship Adam Irad)

;; Query results:
;;
;; ((great grandson) adam irad)


;; Find the relationship between Adam and Jabal
(?relationship Adam Jabal)

;; Query results:
;;
;; ((great great great great great grandson) adam jabal)
