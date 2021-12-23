;; Exercise 3.71
;;
;; Numbers that can be expressed as the sum of two cubes
;; in more than one way are sometimes called Ramanujan numbers,
;; in honor of the mathematician Srinivasa Ramanujan.
;;
;; Ordered streams of pairs provide an elegant solution
;; to the problem of computing these numbers.
;;
;; To find a number that
;; can be written as the sum of two cubes in two different ways,
;; we need only generate the stream of pairs of integers (i,j)
;; weighted according to the sum i^3 + j^3 (see exercise 3.70), then
;; search the stream for two consecutive pairs with the same weight.
;;
;; Write a procedure to generate the Ramanujan numbers.
;; The first such number is 1,729. What are the next five?

(load "workbook.scm")
(load "ex-3.70.scm")

(define (ramanujan-weight pair)
  (+ (expt (car pair) 3)
     (expt (cadr pair) 3)))

;; (ramanujan-weight (list 1 12)) ; 1729
;; (ramanujan-weight (list 9 10)) ; 1729


(define ramanujan-pairs
  (weighted-pairs integers integers ramanujan-weight))

(define ramanujan-candidates
  (stream-map ramanujan-weight ramanujan-pairs))

;; (stream-ref ramanujan-candidates 0) ; 2
;; (stream-ref ramanujan-candidates 1) ; 9
;; (stream-ref ramanujan-candidates 3) ; 28
;; (stream-ref ramanujan-candidates 4) ; 35
;; (stream-ref ramanujan-candidates 5) ; 54
;;
;; (stream-ref ramanujan-candidates 60) ; 1729
;; (stream-ref ramanujan-candidates 61) ; 1729


(define (select-ramanujan candidates)
  (let ((x1 (stream-ref candidates 0))
        (x2 (stream-ref candidates 1)))
    (if (= x1 x2)
        (cons-stream
         x2
         (select-ramanujan (stream-cdr (stream-cdr candidates))))
        (select-ramanujan (stream-cdr candidates)))))

(define ramanujan-numbers
  (select-ramanujan ramanujan-candidates))

;; (stream-ref ramanujan-numbers 0) ; 1729
;; (stream-ref ramanujan-numbers 1) ; 4104
;; (stream-ref ramanujan-numbers 2) ; 13832
;; (stream-ref ramanujan-numbers 3) ; 20683
;; (stream-ref ramanujan-numbers 4) ; 32832
;; (stream-ref ramanujan-numbers 5) ; 39312
