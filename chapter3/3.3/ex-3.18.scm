;; Exercise 3.18
;;
;; Write a procedure that examines a list and determines whether
;; it contains a cycle, that is, whether a program that tried to find
;; the end of the list by taking successive cdrs would go into an infinite loop.
;; Exercise 3.13 constructed such lists.


(load "ex-3.17.scm")

;; The logic is similar to what's been implemented in exercise 3.17.
;;
;; The procedure contains-cycle? keeps track of those items that
;; have already been processed by accumulating them in a list,
;; that is kept as internal state.

(define (contains-cycle? x)
  (let ((pairs (cons '() '())))
    (define (inner l)
      (cond ((null? l) false)
            ((contains? pairs (car l)) true)
            (else
             (adjoin-pair pairs (car l))
             (inner (cdr l)))))
    (inner x)))


;; (define l1 (list 'a 'b 'c))
;; (define l2 (list 'd 'e 'f))

;; (contains-cycle? l1) ; false
;; (contains-cycle? l2) ; false

;; (set-cdr! (cddr l2) l2) ; <- Introduce a cycle for l2

;; (contains-cycle? l1) ; false
;; (contains-cycle? l2) ; true
