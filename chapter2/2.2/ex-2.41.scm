;; Exercise 2.41
;;
;; Write a procedure to find all ordered triples of distinct positive integers i, j, and k
;; less than or equal to a given integer n that sum to a given integer s.

(load "workbook.scm")


;; The task can be split in 2 subtasks:
;; 1) generate a sequence of triples of distinct positive integers
;; 2) filter the sequence of triples to find those whose sum is equal to a given number


;; Start with a procedure that will generate a sequence of triples
;; of distinct positive integers i, j, and k <= to a given integer n:

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; (unique-triples 4) ; ((3 2 1) (4 2 1) (4 3 1) (4 3 2))


;; The following procedure creates a filter predicate - a function that takes a triple
;; as argument and returns true if its sum is equal to a given number:

(define (equal-sum-predicate s)
  (lambda (triple)
    (= (+ (car triple)
          (cadr triple)
          (caddr triple))
       s)))

;; ((equal-sum-predicate 5) (list 3 2 1)) ; false
;; ((equal-sum-predicate 6) (list 3 2 1)) ; true


;; Combining all these definitions yields the complete procedure:

(define (unique-equal-sum-triples n s)
  (filter (equal-sum-predicate s)
          (unique-triples n)))

;; (unique-equal-sum-triples 3 5)   ; ()
;; (unique-equal-sum-triples 3 6)   ; ((3 2 1))
;; (unique-equal-sum-triples 5 8)   ; ((4 3 1) (5 2 1))
;; (unique-equal-sum-triples 10 23) ; ((9 8 6) (10 7 6) (10 8 5) (10 9 4))
;; (unique-equal-sum-triples 10 27) ; ((10 9 8))
