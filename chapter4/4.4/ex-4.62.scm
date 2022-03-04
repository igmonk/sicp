;; Exercise 4.62
;;
;; Define rules to implement the last-pair operation of exercise 2.17,
;; which returns a list containing the last element of a nonempty list.
;;
;; Check your rules on queries such as
;; - (last-pair (3) ?x)
;; - (last-pair (1 2 3) ?x)
;; - (last-pair (2 ?x) (3))
;;
;; Do your rules work correctly on queries such as
;; - (last-pair ?x (3)) ?


(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; The problem can be decomposed in 3 constituents,
;;
;; 1. The given list is empty
;; 2. The given list contains one element
;; 3. None of the above.
;;    In this case a simple fact can be used:
;;    the last pair of a non-empty list with > 1 element
;;    is the last pair of its cdr.


;; 1. The given list is empty
(assert!
 (rule (last-pair () ())))


;; 2. The given list contains one element
(assert!
 (rule (last-pair (?u) (?u))))

;; Alternatively, set the empty list explicitly:
;;
;; (assert!
;;  (rule (last-pair (?u . ()) (?u . ()))))


;; 3. The given list contains > 1 element
(assert!
 (rule (last-pair (?u . ?v) (?z))
       (last-pair ?v (?z))))

;; Alternatively, set the empty list explicitly:
;;
;; (assert!
;;  (rule (last-pair (?u . ?v) (?z . ()))
;;        (last-pair ?v (?z . ()))))


;; Tests

(last-pair () ?x)

;; Query results:
;;
;; (last-pair () ())


(last-pair (3) ?x)

;; Query results:
;;
;; (last-pair (3) (3))


(last-pair (1 2 3) ?x)

;; Query results:
;;
;; (last-pair (1 2 3) (3))


(last-pair (2 ?x) (3))

;; Query results:
;;
;; (last-pair (2 3) (3))


(last-pair ?x (3))

;; The endless recursion...
;;
;; This must be due to the infinite number of lists
;; that comply with the rule and the given query.
