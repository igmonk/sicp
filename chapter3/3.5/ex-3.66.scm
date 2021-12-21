;; Exercise 3.66
;;
;; Examine the stream (pairs integers integers).
;;
;; Can you make any general comments about the order in which
;; the pairs are placed into the stream?
;;
;; For example, about how many pairs precede the pair (1,100)?
;; the pair (99,100)? the pair (100,100)?
;;
;; (If you can make precise mathematical statements here, all the better.
;; But feel free to give more qualitative answers if you find yourself
;; getting bogged down.)

(load "workbook.scm")

(define int-pairs (pairs integers integers))

;; (stream-ref int-pairs 0)  ; (1 1)  <- 1st row
;; (stream-ref int-pairs 1)  ; (1 2)  <- 1st row
;; (stream-ref int-pairs 2)  ; (2 2)     <- 2nd row
;; (stream-ref int-pairs 3)  ; (1 3)  <- 1st row
;; (stream-ref int-pairs 4)  ; (2 3)     <- 2nd row
;; (stream-ref int-pairs 5)  ; (1 4)  <- 1st row
;; (stream-ref int-pairs 6)  ; (3 3)        <- 3rd row
;; (stream-ref int-pairs 7)  ; (1 5)  <- 1st row
;; (stream-ref int-pairs 8)  ; (2 4)     <- 2nd row
;; (stream-ref int-pairs 9)  ; (1 6)  <- 1st row
;; (stream-ref int-pairs 10) ; (3 4)        <- 3rd row
;; (stream-ref int-pairs 11) ; (1 7)  <- 1st row
;; (stream-ref int-pairs 12) ; (2 5)     <- 2nd row
;; (stream-ref int-pairs 13) ; (1 8)  <- 1st row
;; (stream-ref int-pairs 14) ; (4 4)           <- 4th row
;; (stream-ref int-pairs 15) ; (1 9)  <- 1st row
;; (stream-ref int-pairs 16) ; (2 6)     <- 2nd row
;; (stream-ref int-pairs 17) ; (1 10) <- 1st row
;; (stream-ref int-pairs 18) ; (3 5)        <- 3rd row
;; (stream-ref int-pairs 19) ; (1 11) <- 1st row


;; Given the examination above, we can conclude that
;; after their corresponding initial occurences,
;; the pairs (1 n), (2 n), (3 n), ..., (m n) appear
;; in the resulting stream at 2n, 4n, 8n, ..., m*n positions,
;; respectively.
;;
;; Or, more generally, at the position is defined as
;;
;;   2^m * n
;;
;; Such a behaviour is dictated by the procedure 'interleave'.


;; Below are some deliberations and mathematical statements aiming to
;; find the formula to compute the precise index of a pair of integers
;; in the resulting stream.
;;
;; The int-pairs stream is composed of three parts (see workbook):
;; 1 - the pair (S0,T0)
;; 2 - the rest of the pairs in the first row,
;; 3 - the remaining pairs
;;
;; (S0,T0) | (S0,T1) (S0,T2) ...
;; --------|--------------------
;;         | (S1,T1) (S1,T2) ...
;;         |         (S2,T2) ...
;;         |                 ...
;;
;; Given the way the procedure interleave decomposes the matrix
;; into tree parts to combine a stream, the following is observed:
;; 1) the element of the row i that appears in the stream first
;;    belongs to the matrix diagonal => i = j
;; 2) the index of a pair that belongs to the diagonal
;;    can be found by using the formula [1]
;; 3) the index of a pair that does not belong to the diagonal
;;    can be computed in relation to the pair that belongs
;;    to the same row and the matrix diagonal [4]
;; 4) the offset (between two pairs in the same row, one of which
;;    belongs to the diagonal) is composed of two components:
;;    1: the length between the two first pairs in a row [2]
;;    2: the length between the other pairs in a row [3]
;;
;; Formulas:
;;
;;   [1]: 2^(i+1)  [index, diagonal]
;;   [2]: 2^i
;;   [3]: 2^(i+1) * (j-i-1)
;;
;; Derived formulas:
;;
;;   [4] = [2] + [3] = 2^i + 2^(i+1) * (j-i-1)            [offset]
;;   [5] = [1] + [4] = 2^(i+1) + 2^i + 2^(i+1) * (j-i-1)  [index, non-diagonal]
;;
;; Where:
;;   i, j are matrix indices, 0-based.
;;
;; Hence, the resulting function that computes the index
;; of a given pair (which is also the number of preceeding pairs)
;; is a piecewise function:
;; it makes use of the formula [1] to compute the index
;; for a pair that belongs to the matrix diagonal, and
;; the formula [5] to compute that for a non-diagonal pair.

(define (ij-index i j)
  (let ((diagonal-idx (- (expt 2 (+ i 1)) 2)))
    (if (= i j)
        diagonal-idx
        (+ diagonal-idx
           (expt 2 i)
           (* (- j i 1)
              (expt 2 (+ i 1)))))))

;; (ij-index 0 0)  ; 0
;; (ij-index 0 1)  ; 1
;; (ij-index 1 1)  ; 2
;; (ij-index 0 2)  ; 3
;; (ij-index 1 2)  ; 4
;; (ij-index 0 3)  ; 5
;; (ij-index 2 2)  ; 6
;; (ij-index 0 4)  ; 7
;; (ij-index 1 3)  ; 8
;; (ij-index 0 5)  ; 9
;; (ij-index 2 3)  ; 10
;; (ij-index 0 6)  ; 11
;; (ij-index 1 4)  ; 12
;; (ij-index 0 7)  ; 13
;; (ij-index 3 3)  ; 14
;; (ij-index 0 8)  ; 15
;; (ij-index 1 5)  ; 16
;; (ij-index 0 9)  ; 17
;; (ij-index 2 4)  ; 18
;; (ij-index 0 10) ; 19

(define (pair-index pair)
  (ij-index (- (car pair) 1)
            (- (cadr pair) 1)))

;; (pair-index (list 1 1))  ; 0
;; (pair-index (list 1 2))  ; 1
;; (pair-index (list 2 2))  ; 2
;; (pair-index (list 1 3))  ; 3
;; (pair-index (list 2 3))  ; 4
;; (pair-index (list 1 4))  ; 5
;; (pair-index (list 3 3))  ; 6
;; (pair-index (list 1 5))  ; 7
;; (pair-index (list 2 4))  ; 8
;; (pair-index (list 1 6))  ; 9
;; (pair-index (list 3 4))  ; 10
;; (pair-index (list 1 7))  ; 11
;; (pair-index (list 2 5))  ; 12
;; (pair-index (list 1 8))  ; 13
;; (pair-index (list 4 4))  ; 14
;; (pair-index (list 1 9))  ; 15
;; (pair-index (list 2 6))  ; 16
;; (pair-index (list 1 10)) ; 17
;; (pair-index (list 3 5))  ; 18
;; (pair-index (list 1 11)) ; 19


;; How many pairs precede the pairs (1,100), (99,100), (100,100)?
;;
;; (pair-index (list 1 100))   ; 197
;; (pair-index (list 99 100))  ; 950737950171172051122527404030
;; (pair-index (list 100 100)) ; 1267650600228229401496703205374
;;
;; The resulting number denotes both the index of a given pair
;; and the number of pairs preceeding it.
