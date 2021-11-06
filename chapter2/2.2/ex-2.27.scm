;; Exercise 2.27
;;
;; Modify your 'reverse' procedure of exercise 2.18 to produce a 'deep-reverse' procedure
;; that takes a list as argument and returns as its value the list with its elements reversed
;; and with all sublists deep-reversed as well.

(load "ex-2.18.scm")


;; There are two base cases:
;;   1) deep-reverse of the empty list/tree is the empty list/tree
;;   2) deep-reverse of a leaf is the leaf itself
;; and one reduction step, where we strip off the 'car' of the list.
;; It must be taken into account that the 'car' may itself be a tree, whose leaves
;; we need to reverse. Thus, the appropriate reduction step is:
;;   - deep-reverse of a tree x is deep-reverse of the car of x prepended to
;;     deep-reverse of the cdr of x.


;; 1. Recursive without append
;;
;; Below, 'deep-reverse' is implemented without using the procedure append.
;; To reverse list/tree x, do the following:
;;   - if x is the empty list/tree, then the result is the accumulator 'result'
;;   - if x is a leaf, then the result is the leaf itself
;;   - otherwise, cdr x and append deep-reverse of the car of x onto the accumulator 'result'
;;
;; The initial value of the accumulator 'result' is the empty list.
;;
;; The only difference between 'reverse' procedure of exercise 2.18 and 'deep-reverse' is
;; the fact that the 'car' may itself be a tree, whose leaves we need to reverse.

(define (deep-reverse x)
  (define (iter l result)
    (cond ((null? l) result)
          ((not (pair? l)) l)
          (else (iter (cdr l)
                      (cons (iter (car l) '())
                            result)))))
  (iter x '()))


;; 2. Recursive
;;
;; Below is another implementation of 'deep-reverse', that makes use of the 'append' procedure
;; and generate a recursive process.

(define (deep-reverse x)
  (cond ((null? x) x)
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))


(define x (list (list 1 2) (list 3 4)))

;; x                ; ((1 2) (3 4))
;; (reverse x)      ; ((3 4) (1 2))
;; (deep-reverse x) ; ((4 3) (2 1))

(define xx (list (list 1 2) (list 3 4) (list 5 6)))

;; xx                ; ((1 2) (3 4) (5 6))
;; (reverse xx)      ; ((5 6) (3 4) (1 2))
;; (deep-reverse xx) ; ((6 5) (4 3) (2 1))

(define xxx (list (list (list 1 2) (list 3 4))
                  (list (list 5 6) (list 7 8))))

;; xxx                ; (((1 2) (3 4)) ((5 6) (7 8)))
;; (reverse xxx)      ; (((5 6) (7 8)) ((1 2) (3 4)))
;; (deep-reverse xxx) ; (((8 7) (6 5)) ((4 3) (2 1)))

(define y (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;; y ; ((1 2 3) (4 5 6) (7 8 9))
;; (reverse y) ; ((7 8 9) (4 5 6) (1 2 3))
;; (deep-reverse y) ; ((9 8 7) (6 5 4) (3 2 1))
