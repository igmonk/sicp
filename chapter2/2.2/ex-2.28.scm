;; Exercise 2.28
;;
;; Write a procedure 'fringe' that takes as argument a tree (represented as a list) and
;; returns a list whose elements are all the leaves of the tree arranged in left-to-right order.

(load "workbook.scm")

(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))


(define x (list (list 1 2) (list 3 4)))

;; x          ; ((1 2) (3 4))
;; (fringe x) ; (1 2 3 4)

(define xx (list (list 1 2) (list 3 4) (list 5 6)))

;; xx          ; ((1 2) (3 4) (5 6))
;; (fringe xx) ; (1 2 3 4 5 6)

(define xxx (list (list (list 1 2) (list 3 4))
                  (list (list 5 6) (list 7 8))))

;; xxx          ; (((1 2) (3 4)) ((5 6) (7 8)))
;; (fringe xxx) ; (1 2 3 4 5 6 7 8)
