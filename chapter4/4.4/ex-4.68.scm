;; Exercise 4.68
;;
;; Define rules to implement the reverse operation of exercise 2.18,
;; which returns a list containing the same elements as a given list
;; in reverse order.
;;
;; (Hint: Use append-to-form.)
;;
;; Can your rules answer both (reverse (1 2 3) ?x) and
;; (reverse ?x (1 2 3)) ?

(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; Below are the constituents of the 'append to form' rule.

(assert!
 (rule (append-to-form () ?y ?y)))

(assert!
 (rule (append-to-form (?u . ?v) ?y (?u . ?z))
       (append-to-form ?v ?y ?z)))


;; Below are the constituents of the 'reverse' rule.
;;
;; The empty list case

(assert!
 (rule (reverse () ())))


;; Option 1: cdr-ing down the first list

(assert!
 (rule (reverse (?u . ?v) ?z)
       (and (reverse ?v ?y)
            (append-to-form ?y (?u) ?z))))

;; (reverse () ?x) ; (reverse () ())
;; (reverse ?x ()) ; infinite loop

;; (reverse (1 2 3) ?x) ; (reverse (1 2 3) (3 2 1))
;; (reverse ?x (1 2 3)) ; infinite loop


;; Option 2: cdr-ing down the second list

(assert!
 (rule (reverse ?z (?u . ?v))
       (and (reverse ?y ?v)
            (append-to-form ?y (?u) ?z))))

;; (reverse () ?x) ; infinite loop
;; (reverse ?x ()) ; (reverse () ())

;; (reverse (1 2 3) ?x) ; infinite loop
;; (reverse ?x (1 2 3)) ; (reverse (3 2 1) (1 2 3))
