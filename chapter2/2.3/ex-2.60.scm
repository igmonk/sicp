;; Exercise 2.60
;;
;; We specified that a set would be represented as a list with no duplicates.
;; Now suppose we allow duplicates.
;;
;; For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2).
;;
;; Design procedures element-of-set?, adjoin-set, union-set, and intersection-set
;; that operate on this representation.
;;
;; How does the efficiency of each compare with the corresponding procedure
;; for the non-duplicate representation?
;;
;; Are there applications for which you would use this representation in preference to
;; the non-duplicate one?


(load "../../common.scm")
(load "workbook.scm")


;; 1. Procedures

;; element-of-set? stays the same

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; intersection-set stays the same


;; 2. Efficiency

;; adjoin-set and union-set that allow for duplicates are more efficient than
;; their non-duplicate counterparts, since there is no need in element-of-set check.
;; The number of steps required grows as θ(1).
;;
;; Since element-of-set? and intersection-set are the same for both representations,
;; their number of steps required is also the same - θ(n) and θ(n^2), accordingly,
;; However, the efficiency of the representation that allows for duplicates
;; will strongly depend on the patterns of data, or, more formally, on the multiplicity
;; of the elements in a multiset.


;; 3. Preference

;; For those applications that rely on the uniquness of each element,
;; the choice is obvious - the non-duplicate representation of sets.
;;
;; If an application is indifferent to the presence of duplicates,
;; it could benefit from the duplicate representation of sets in case
;; the prevailing number of operations are adjoin-set and union-set.
