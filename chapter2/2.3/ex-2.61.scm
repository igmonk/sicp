;; Exercise 2.61
;;
;; Give an implementation of adjoin-set using the ordered representation.
;;
;; By analogy with element-of-set? show how to take advantage of
;; the ordering to produce a procedure that requires on the average
;; about half as many steps as with the unordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))))


;; (define s1 '(1 3 5))
;;
;; (adjoin-set 0 s1) ; (0 1 3 5)
;; (adjoin-set 1 s1) ; (1 3 5)
;; (adjoin-set 2 s1) ; (1 2 3 5)
;; (adjoin-set 3 s1) ; (1 3 5)
;; (adjoin-set 4 s1) ; (1 3 4 5)
;; (adjoin-set 5 s1) ; (1 3 5)
;; (adjoin-set 6 s1) ; (1 3 5 6)
