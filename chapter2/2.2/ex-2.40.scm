;; Exercise 2.40
;;
;; Define a procedure 'unique-pairs' that, given an integer n,
;; generates the sequence of pairs (i,j) with 1 <= j < i <= n.
;;
;; Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(load "../../common.scm")
(load "workbook.scm")

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; (unique-pairs 3) ; ((2 1) (3 1) (3 2))
;; (unique-pairs 4) ; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; (prime-sum-pairs 6)
;;
;; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
