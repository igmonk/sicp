;; Exercise 2.65
;;
;; Use the results of exercises 2.63 and 2.64 to give θ(n) implementations of
;; union-set and intersection-set for sets implemented as (balanced) binary trees.

(load "workbook.scm")
(load "ex-2.62.scm")
(load "ex-2.63.scm")
(load "ex-2.64.scm")


;; The combination of the following procedures gives us a procedure
;; that computes the union of two sets implemented as binary trees:
;;
;; 1) tree->list-1 - converts a binary tree to a list, θ(n)
;; 2) union-set    - computes the union of two sets, θ(n)
;; 3) list->tree   - converts an ordered list to a balanced binary tree, θ(n)
;;
;; The order of growth in the number of steps required by union-set-bt is θ(n).

(define (union-set-bt set1 set2)
  (list->tree
   (union-set (tree->list-1 set1)
              (tree->list-1 set2))))

;; (union-set-bt '() '()) ; ()
;;
;; (union-set-bt (list->tree '(1 2 3)) '()) ; (2 (1 () ()) (3 () ()))
;; (union-set-bt '() (list->tree '(1 2 3))) ; (2 (1 () ()) (3 () ()))
;;
;; (union-set-bt (list->tree '(1 2 3))
;;               (list->tree '(1 2 3))) ; (2 (1 () ()) (3 () ()))
;;
;; (union-set-bt (list->tree '(1 3 5))
;;               (list->tree '(7 9 11))) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))


;; The combination of the following procedures gives us a procedure
;; that computes the intersection of two sets implemented as binary trees:
;;
;; 1) tree->list-1     - converts a binary tree to a list, θ(n)
;; 2) intersection-set - computes the intersection of two sets, θ(n)
;; 3) list->tree       - converts an ordered list to a balanced binary tree, θ(n)
;;
;; The order of growth in the number of steps required by intersection-set-bt is θ(n).

(define (intersection-set-bt set1 set2)
  (list->tree
   (intersection-set (tree->list-1 set1)
                     (tree->list-1 set2))))

;; (intersection-set-bt '() '()) ; ()
;;
;; (intersection-set-bt (list->tree '(1 2 3)) '()) ; ()
;; (intersection-set-bt '() (list->tree '(1 2 3))) ; ()
;;
;; (intersection-set-bt (list->tree '(1 2 3))
;;                      (list->tree '(1 2 3))) ; (2 (1 () ()) (3 () ()))
;;
;; (intersection-set-bt (list->tree '(1 2 3 4 5 6 7 8 9 10 11))
;;                      (list->tree '(1 3 5 7 9 11))) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
