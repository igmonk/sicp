;; Exercise 2.32
;;
;; We can represent a set as a list of distinct elements, and we can represent
;; the set of all subsets of the set as a list of lists.
;;
;; For example, if the set is (1 2 3), then the set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
;;
;; Complete the following definition of a procedure that generates the set of subsets of a set
;; and give a clear explanation of why it works:
;;
;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))

(load "workbook.scm")

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((cdr-subsets (subsets (cdr s))))
        (append cdr-subsets
                (map (lambda (x) (cons (car s) x))
                     cdr-subsets)))))

;; (subsets '())      ; (())
;; (subsets '(1))     ; (() (1))
;; (subsets '(1 2))   ; (() (2) (1) (1 2))
;; (subsets '(1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; The set of all subsets of a given set S is the union of:
;; 1) S'  - the set of all subsets of the set S without its element S(n) and
;; 2) S'' - the set containing all the elements of the set S' enriched with the element S(n)
;;
;; The procedure above generates a recursive process, where with each recursive call
;; the length of the list is reduced by one and its head is left aside for a deferred
;; operation. The base case of the process is the empty list, which results in a list
;; containing the empty list.
;;
;; As the recursion process folds back, the deferred operation applies mapping,
;; whose goal is to iterate over the set of all recursively generated subsets, enrich
;; them with the head element that was left aside and create a union of these two sets. 
