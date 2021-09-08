;; Exercise 2.22
;;
;; Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21
;; so that it evolves an iterative process:

(load "../../common.scm")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; Unfortunately, defining square-list this way produces the answer list
;; in the reverse order of the one desired. Why?
;;
;; (square-list (list 1 2 3 4)) ; (16 9 4 1)

;; square-list defined above produces the answer list in the reserve order
;; of the one desired simply because each subsequent element, which corresponds to
;; the square of its original operand, is prepended to the answer list.

;; Louis then tries to fix his bug by interchanging the arguments to 'cons':

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; This doesn't work either. Explain.
;;
;; (square-list (list 1 2 3 4)) ; ((((() . 1) . 4) . 9) . 16)

;; The rearrangement of the cons operands did not solve the issue,
;; since the sequence of cons operations builds a version of a flat list
;; only when each subsequent element is prepended to the existing list,
;; not vice versa.
