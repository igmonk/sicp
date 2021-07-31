;; Exercise 1.12
;;
;; Write a procedure that computes elements of Pascal's triangle
;; by means of a recursive process.

;; Let n and k be a row and column of Pascal's triangle, respectively.
;;
;; Then, the entry in the n-th row and k-th column is defined by:
;; (n, k) = (n-1, k-1) + (n-1, k)
;;
;; for any non-negative integer n and any integet 0 <= k <= n
;;
;; The unique nonzero entry in the topmost row is (n=0, k=0) = 1.
;;
;; https://en.wikipedia.org/wiki/Pascal%27s_triangle

(define (pt n k)
  (cond ((or (< k 0) (> k n)) 0)
	((or (= n 0) (= k 0)) 1)
	(else (+ (pt (- n 1) (- k 1))
		 (pt (- n 1) k)))))

;; (pt 0 0) ; 0

;; (pt 1 0) ; 1
;; (pt 1 1) ; 1

;; (pt 2 0) ; 1
;; (pt 2 1) ; 2
;; (pt 2 2) ; 1

;; (pt 3 0) ; 1
;; (pt 3 1) ; 3
;; (pt 3 2) ; 3
;; (pt 3 3) ; 1

;; (pt 4 0) ; 1
;; (pt 4 1) ; 4
;; (pt 4 2) ; 6
;; (pt 4 3) ; 4
;; (pt 4 4) ; 1

;; (pt 5 0) ; 1
;; (pt 5 1) ; 5
;; (pt 5 2) ; 10
;; (pt 5 3) ; 10
;; (pt 5 4) ; 5
;; (pt 5 5) ; 1
