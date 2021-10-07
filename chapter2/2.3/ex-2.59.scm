;; Exercise 2.59
;;
;; Implement the union-set operation for the unordered-list representation of sets.

(load "workbook.scm")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))


;; (union-set '() '()) ; ()

;; (union-set '() '(1 2 3)) ; (1 2 3)
;; (union-set '(1 2 3) '()) ; (1 2 3)

;; (union-set '(1 2 3) '(1 2 3)) ; (1 2 3)
;; (union-set '(1 2 3) '(2 3 4)) ; (1 2 3 4)
;; (union-set '(1 2 3) '(3 4 5)) ; (1 2 3 4 5)
;; (union-set '(1 2 3) '(4 5 6)) ; (1 2 3 4 5 6)
