;; Exercise 3.17
;;
;; Devise a correct version of the count-pairs procedure of exercise 3.16
;; that returns the number of distinct pairs in any structure.
;;
;; (Hint: Traverse the structure, maintaining an auxiliary data structure
;; that is used to keep track of which pairs have already been counted.)


(load "ex-3.16.scm")

;; The following procedure returns the number of distinct pairs in any structure.
;;
;; The idea is to keep an internal state and updated it each time a new pair
;; is encountered by adjoining the current pair to the set of those already
;; counted.
;;
;; The need for that internal state comes from the fact that the structure
;; to be traversed is a tree, where a node can be referenced more than once,
;; which, when processed, generates a recursive plan.
;; 
;; All the branches of the plan must check if the current pair has already
;; been processed. That information is kept in the internal state.
;;
;; The adjoin operation is applied to the internal state and a new pair.
;; The operation is described in details below.

(define (count-pairs x)
  (let ((counted-pairs (cons '() '())))
    (define (inner p)
      (cond ((not (pair? p)) 0)
            ((contains? counted-pairs p) 0)
            (else
             (adjoin-pair counted-pairs p)
             (+ (inner (car p))
                (inner (cdr p))
                1))))
    (inner x)))

;; (count-pairs s3)    ; 3
;; (count-pairs s4)    ; 3
;; (count-pairs s5)    ; 3
;; (count-pairs s7)    ; 3
;; (count-pairs s-inf) ; 3


;; The adjoin operation updates the car and cdr parts of the first pair:
;; - its cdr is assigned a new pair formed by cons-ing the car of the pair
;;   together with its cdr;
;; - its car is assigned the second pair.
;;
;; The logic can be thought of as an application of the right-shift operation
;; applied to the elements of the first pair, followed by initializing
;; the deallocated space (the car of the first pair) with the second pair.

(define (adjoin-pair p1 p2)
  (set-cdr! p1 (cons (car p1) (cdr p1)))
  (set-car! p1 p2))

;; (define p0 (cons '() '()))
;; (define p1 (cons 1 2))
;; (define p2 (cons 3 4))

;; p0 ; (())
;; p1 ; (1 . 2)
;; p2 ; (3 . 4)

;; (adjoin-pair p0 p1)
;; p0 ; ((1 . 2) ())

;; (adjoin-pair p0 p2)
;; p0 ; ((3 . 4) (1 . 2) ())


(define (contains? s e)
  (cond ((null? s) false)
        ((equal? (car s) e) true)
        (else
         (contains? (cdr s) e))))

;; (contains? (list s3 s4 s5 s7) s7)    ; true
;; (contains? (list s3 s4 s5 s7) s-inf) ; false
