;; Exercise 2.37
;;
;; Suppose we represent vectors v=(v[i]) as sequences of numbers,
;; and matrices m = (m[ij]) as sequences of vectors (the rows of the matrix).
;;
;; For example, the matrix
;;
;; | 1 2 3 4 |
;; | 4 5 6 6 |
;; | 6 7 8 9 |
;;
;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).
;;
;; With this representation, we can use sequence operations to concisely express
;; the basic matrix and vector operations.
;; These operations (which are described in any book on matrix algebra) are the following:
;; - (dot-product v w)     returns the sum Σ[i](v[i]w[i])
;; - (matrix-*-vector m v) returns the vector t, where t[i] = Σ[j](m[ij]v[j])
;; - (matrix-*-matrix m n) returns the matrix p, where p[ij] = Σ[k](m[ik]n[kj])
;; - (transpose m)         returns the matrix n, where n[ij] = m[ji]

;; Prerequisite: extended version of map.
;;
;; Scheme standardly provides a 'map' procedure that is more general than
;; the one defined in the workbook.
;; This more general map takes a procedure of n arguments, together with n lists,
;; and applies the procedure to all the first elements of the lists,
;; all the second elements of the lists, and so on, returning a list of the results.
;;
;; For example:
;;
;; (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
;; (741 852 963)
;;
;; Source:
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#footnote_Temp_166

(define _map map)


;; 1. Dot product
;;
;; We can define the dot product as:
;; (this definition uses the extended version of 'map')

(load "workbook.scm")

(define (dot-product v w)
  (accumulate + 0 (_map * v w)))

;; (dot-product (list 1 3 -5) (list 4 -2 -1)) ; 3

;; Fill in the missing expressions in the following procedures for computing
;; the other matrix operations.


;; 2. Matrix * vector

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

;; (define m1 (list (list 1 2 3 4)
;;                  (list 4 5 6 6)
;;                  (list 6 7 8 9)))

;; (define v1 (list 1 3 5 7))

;; (matrix-*-vector m1 v1) ; (50 91 130)


;; 3. Transpose

(load "ex-2.36.scm")

(define (transpose m)
  (accumulate-n cons '() m))

;; (transpose (list (list 1 2) (list 3 4) (list 5 6))) ; ((1 3 5) (2 4 6))
;; (transpose (list (list 1 3 5) (list 2 4 6)))        ; ((1 2) (3 4) (5 6))
;; (transpose m1)                                      ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))


;; 4. Matrix * matrix

;; The implementation below doesn't work, since it relies on matrix-*-vector,
;; which returns a row (a list) even when it is supposed to be a column (a list of lists).
;;
;; (define (matrix-*-matrix m n)
;;   (map (lambda (n-col)
;;          (matrix-*-vector m n-col))
;;        (transpose n)))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                n-cols))
         m)))

;; (matrix-*-matrix m1 (transpose m1)) ; ((30 56 80) (56 113 161) (80 161 230))
