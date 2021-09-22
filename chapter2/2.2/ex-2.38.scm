;; Exercise 2.38
;;
;; The 'accumulate' procedure is also known as 'fold-right', because it combines
;; the first element of the sequence with the result of combining all the elements
;; to the right.

(load "workbook.scm")

(define fold-right accumulate)

;; There is also a 'fold-left', which is similar to 'fold-right', except that
;; it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of
;;
;; (fold-right / 1 (list 1 2 3)) ; 3/2
;; (fold-left / 1 (list 1 2 3))  ; 1/6
;;
;; (fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
;; (fold-left list '() (list 1 2 3))  ; (((() 1) 2) 3)

;; Give a property that 'op' should satisfy to guarantee that fold-right and fold-left
;; will produce the same values for any sequence.


;; Not only do fold-right and fold-left apply 'op' in reverse order,
;; but their accordinate accumulated values are passed to 'op'
;; as the second and first arguments, respectively.
;;
;; Effectively, the are two major differences between the fold-right and
;; fold-left procedures:
;; - mutually reversed order of 'op' applications
;; - mutually reversed order of the operands of 'op'
;;
;; In order to guarantee that fold-right and fold-left produce the same values
;; for any sequence, 'op' should possess the following properties:
;; - the associative property to nullify the different order of the applications of 'op'
;; - the commutative property to nullify the different order of the operands of 'op'


;; Test 1: 'op' is neither associative nor commutative
;;
;; (fold-right - 1 (list 2 3 4)) ; 2
;; (fold-left - 1 (list 2 3 4))  ; -8


;; Test 2: 'op' is associative, but not commutative
;;
;; Operations such as function composition and matrix multiplication are associative,
;; but (generally) not commutative.
;;
;; (load "ex-2.37.scm")
;;
;; (define m1 (list (list 1 2) (list 3 4)))
;; (define m2 (list (list 5 6) (list 7 8)))
;; (define m3 (list (list 9 10) (list 11 12)))
;;
;; below is shown that the associative property holds for matrix multiplication
;; whereas the commutative one does not:
;;
;; m1 * m2 * m3  =  m1 * (m2 * m3)  !=  m1 * m3 * m2
;;
;; m1 * m2 * m3
;; (matrix-*-matrix (matrix-*-matrix m1 m2) m3) ; ((413 454) (937 1030))
;;
;; m1 * (m2 * m3)
;; (matrix-*-matrix m1 (matrix-*-matrix m2 m3)) ; ((413 454) (937 1030))
;;
;; m1 * m3 * m2
;; (matrix-*-matrix (matrix-*-matrix m1 m3) m2) ; ((393 458) (901 1050))
;;
;; m1 * (m3 * m2)
;; (matrix-*-matrix m1 (matrix-*-matrix m3 m2)) ; ((393 458) (901 1050))
;;
;;
;; The absence of the commutative property is outlined below:
;;
;; m1 * m2
;; (matrix-*-matrix m1 m2) ; ((19 22) (43 50))
;; 
;; m2 * m1
;; (matrix-*-matrix m2 m1) ; ((23 34) (31 46))

;; Therefore, given the same initial value and sequence,
;; fold-right and fold-left result in different matrices:
;;
;; initial: m1, sequence: (m2, m3)
;;
;; (fold-right matrix-*-matrix m1 (list m2 m3)) ; ((543 737) (655 889))
;; (fold-left matrix-*-matrix m1 (list m2 m3))  ; ((644 748) (706 820))
;;
;; In case there is only one matrix involved, the result is the same:
;;
;; initial:m1, sequence: (m1, m1)
;;
;; (fold-right matrix-*-matrix m1 (list m1 m1)) ; ((37 54) (81 118))
;; (fold-left matrix-*-matrix m1 (list m1 m1))  ; ((37 54) (81 118))


;; Test 3: 'op' is both associative and commutative
;;
;; (fold-right + 1 (list 2 3 4 5)) ; 15
;; (fold-left + 1 (list 2 3 4 5))  ; 15
;;
;; (fold-right * 1 (list 2 3 4 5)) ; 120
;; (fold-left * 1 (list 2 3 4 5))  ; 120


;; Resources:
;; - https://en.wikipedia.org/wiki/Associative_property
;; - https://en.wikipedia.org/wiki/Commutative_property
