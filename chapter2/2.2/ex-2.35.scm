;; Exercise 2.35
;;
;; Redefine 'count-leaves' from section 2.2.2 as an accumulation.

(load "workbook.scm")


;; Using accumulate

(define (count-leaves tree)
  (accumulate (lambda (sub-tree acc)
                (if (pair? sub-tree)
                    (+ acc (count-leaves sub-tree))
                    (+ acc 1)))
              0
              tree))


;; Using accumulate and map
;;
;; 'map' computes the number of leaves for each sub-tree.
;; 'accumulate' sums up what is computed by 'map'.

(define (count-leaves tree)
  (accumulate (lambda (leaves-count acc)
                (+ acc leaves-count))
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   tree)))


;; (count-leaves (list 1 (list 2 3) (list 4 5 6) 7)) ; 7
