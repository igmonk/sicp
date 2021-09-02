;; Exercise 2.11
;;
;; In passing, Ben also cryptically comments:
;;
;; "By testing the signs of the endpoints of the intervals,
;;  it is possible to break 'mul-interval' into nine cases,
;;  only one of which requires more than two multiplications."
;;
;; Rewrite this procedure using Ben's suggestion.


;; Here are the 9 cases, one of which requires more than two multiplications:
;;
;; lb1 < 0  and ub1 < 0  and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 ub2) (* lb1 lb2))
;; lb1 < 0  and ub1 < 0  and lb2 < 0  and ub2 >= 0 => (make-interval (* lb1 ub2) (* lb1 lb2))
;; lb1 < 0  and ub1 < 0  and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 ub2) (* ub1 lb2))
;; lb1 < 0  and ub1 >= 0 and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 lb2) (* lb1 lb2))
;; lb1 < 0  and ub1 >= 0 and lb2 < 0  and ub2 >= 0 => (make-interval (min (* ub1 lb2) (* lb1 ub2))
;;                                                                   (max (* lb1 lb2) (* ub1 ub2)))
;; lb1 < 0  and ub1 >= 0 and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 ub2) (* ub1 ub2))
;; lb1 >= 0 and ub1 >= 0 and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 lb2) (* lb1 ub2))
;; lb1 > 0  and ub1 >= 0 and lb2 < 0  and ub2 >= 0 => (make-interval (* ub1 lb2) (* ub1 ub2))
;; lb1 >= 0 and ub1 >= 0 and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 lb2) (* ub1 ub2))
;;
;; (the way these cases were found is shown in the comments section below)

(load "workbook.scm")
(load "ex-2.7.scm")

;; Define a representation for a pair of pairs

(define (pp a b c d)
  (cons (cons a b)
        (cons c d)))

(define (ff pp) (car (car pp)))
(define (fs pp) (cdr (car pp)))
(define (sf pp) (car (cdr pp)))
(define (ss pp) (cdr (cdr pp)))

;; (define pp1 (pp 1 2 3 4))
;; (ff pp1) ; 1
;; (fs pp1) ; 2
;; (sf pp1) ; 3
;; (ss pp1) ; 4

;; ------------------------------------------

;; Define procedures that make use of the pair-of-pairs procedure

(define (pp-eq? pp1 pp2)
  (and (eqv? (ff pp1) (ff pp2))
       (eqv? (fs pp1) (fs pp2))
       (eqv? (sf pp1) (sf pp2))
       (eqv? (ss pp1) (ss pp2))))

;; Note:
;;
;; (eqv? < <)   <= Pointer equivalence
;; (equal? < <) <= Structural equivalence

;; (pp-eq? (pp 1 2 3 4) (pp 1 2 3 4)) ; true
;; (pp-eq? (pp 1 2 3 4) (pp 1 3 2 4)) ; false

(define (intervals-signs-pp i1 i2)
  (pp (sign (lower-bound i1))
      (sign (upper-bound i1))
      (sign (lower-bound i2))
      (sign (upper-bound i2))))

;; ------------------------------------------

(define (mul-interval i1 i2)
  (let ((lb1 (lower-bound i1))
        (ub1 (upper-bound i1))
        (lb2 (lower-bound i2))
        (ub2 (upper-bound i2))
        (ipp (intervals-signs-pp i1 i2)))
    (cond ((pp-eq? (pp < < < <) ipp)
           (make-interval (* ub1 ub2) (* lb1 lb2)))
          ((pp-eq? (pp < < < >=) ipp)
           (make-interval (* lb1 ub2) (* lb1 lb2)))
          ((pp-eq? (pp < < >= >=) ipp)
           (make-interval (* lb1 ub2) (* ub1 lb2)))
          ((pp-eq? (pp < >= < <) ipp)
           (make-interval (* ub1 lb2) (* lb1 lb2)))
          ((pp-eq? (pp < >= < >=) ipp)
           (make-interval (min (* ub1 lb2) (* lb1 ub2))
                          (max (* lb1 lb2) (* ub1 ub2))))
          ((pp-eq? (pp < >= >= >=) ipp)
           (make-interval (* lb1 ub2) (* ub1 ub2)))
          ((pp-eq? (pp >= >= < <) ipp)
           (make-interval (* ub1 lb2) (* lb1 ub2)))
          ((pp-eq? (pp >= >= < >=) ipp)
           (make-interval (* ub1 lb2) (* ub1 ub2)))
          ((pp-eq? (pp >= >= >= >=) ipp)
           (make-interval (* lb1 lb2) (* ub1 ub2))))))


;; Tests to check the 9 cases work with the new implementation of mul-interval

;; (define i1 (make-interval -5 -1))
;; (define i2 (make-interval -1 5))
;; (define i3 (make-interval 1 5))

;; (define i11 (make-interval -7 -3))
;; (define i22 (make-interval -3 7))
;; (define i33 (make-interval 3 7))

;; (define i1*i11 (mul-interval i1 i11))
;; (define i1*i22 (mul-interval i1 i22))
;; (define i1*i33 (mul-interval i1 i33))
;; (define i2*i11 (mul-interval i2 i11))
;; (define i2*i22 (mul-interval i2 i22))
;; (define i2*i33 (mul-interval i2 i33))
;; (define i3*i11 (mul-interval i3 i11))
;; (define i3*i22 (mul-interval i3 i22))
;; (define i3*i33 (mul-interval i3 i33))

;; (print-interval i1*i11) ; [3,35]
;; (print-interval i1*i22) ; [-35,15]
;; (print-interval i1*i33) ; [-35,-3]
;; (print-interval i2*i11) ; [-35,7]
;; (print-interval i2*i22) ; [-15,35]
;; (print-interval i2*i33) ; [-7,35]
;; (print-interval i3*i11) ; [-35,-3]
;; (print-interval i3*i22) ; [-15,35]
;; (print-interval i3*i33) ; [3,35]

;; Utils

(define (sign n)
  (if (>= n 0) >= <))


;; Break 'mul-intervals' into 9 cases:
;;
;; 1. Find out all the possible combinations of product signs.
;;
;;    *    | lb2 < 0 | lb2 = 0 | lb2 > 0 | ub2 < 0 | ub2 = 0 | ub2 > 0 |
;; --------|---------|---------|---------|---------|---------|---------|
;; lb1 < 0 |    +    |    0    |    -    |    +    |    0    |    -    |
;; lb1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; lb1 > 0 |    -    |    0    |    +    |    -    |    0    |    +    |
;; ub1 < 0 |    +    |    0    |    -    |    +    |    0    |    -    |
;; ub1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; ub1 > 0 |    -    |    0    |    +    |    -    |    0    |    +    |

;; 2. Going row by row, figure out the relation between absolute values
;;    of the products of the same sign (++ >= + AND -- <= -).
;;
;;    *    | lb2 < 0 | lb2 = 0 | lb2 > 0 | ub2 < 0 | ub2 = 0 | ub2 > 0 |
;; --------|---------|---------|---------|---------|---------|---------|
;; lb1 < 0 |   + +   |    0    |    -    |    +    |    0    |   - -   |
;; lb1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; lb1 > 0 |   - -   |    0    |    +    |    -    |    0    |   + +   |
;; ub1 < 0 |   + +   |    0    |    -    |    +    |    0    |   - -   |
;; ub1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; ub1 > 0 |   - -   |    0    |    +    |    -    |    0    |   + +   |

;; 3. Going column by column, find out the relation between absolute values
;;    of the products of the same sign (+++ >= ++ >= + AND --- <= -- <= -).
;;
;;    *    | lb2 < 0 | lb2 = 0 | lb2 > 0 | ub2 < 0 | ub2 = 0 | ub2 > 0 |
;; --------|---------|---------|---------|---------|---------|---------|
;; lb1 < 0 |  + + +  |    0    |   - -   |   + +   |    0    |  - - -  |
;; lb1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; lb1 > 0 |   - -   |    0    |    +    |    -    |    0    |   + +   |
;; ub1 < 0 |   + +   |    0    |    -    |    +    |    0    |   - -   |
;; ub1 = 0 |    0    |    0    |    0    |    0    |    0    |    0    |
;; ub1 > 0 |  - - -  |    0    |   + +   |   - -   |    0    |  + + +  |


;; lb1 = 0, ub1 = 0, lb2 = 0, ub2 = 0 => (make-interval 0 0)
;;
;; Hence, the table can be reduced as shown below:

;;    *    | lb2 < 0 | lb2 > 0 | ub2 < 0 | ub2 > 0 |
;; --------|---------|---------|---------|---------|
;; lb1 < 0 |  + + +  |   - -   |   + +   |  - - -  |
;; lb1 > 0 |   - -   |    +    |    -    |   + +   |
;; ub1 < 0 |   + +   |    -    |    +    |   - -   |
;; ub1 > 0 |  - - -  |   + +   |   - -   |  + + +  |

;; From this point, we should go through all the possible combinations of
;; lb1, ub1, lb2 and ub2 and find out the way their product is computed.
;;
;; It is important to exclude non-existing cases, where:
;;
;; 1) lb1 > ub1
;; 2) lb2 > ub2

;; For example:
;;
;; lb1 < 0 and ub1 < 0 and lb2 < 0 and ub2 < 0
;;
;;    *    | lb2 < 0 | ub2 < 0 |
;; --------|---------|---------|
;; lb1 < 0 |  + + +  |   + +   |
;; ub1 < 0 |   + +   |    +    |
;;
;; Min product is ub1 * ub2
;; Max product is lb1 * lb2
;;
;; => (make-interval (* ub1 ub2) (* lb1 lb2))
;;
;;
;; Another example:
;;
;; lb1 < 0 and ub1 > 0 and lb2 < 0 and ub2 > 0
;;
;;    *    | lb2 < 0 | ub2 > 0 |
;; --------|---------|---------|
;; lb1 < 0 |  + + +  |  - - -  |
;; ub1 > 0 |  - - -  |  + + +  |
;;
;; It is unclear the products of what bounds are the min and max.
;; Thereby, we need to consider 2 possible cases to sort it out.
;;
;; => (make-interval (min (* ub1 lb2) (* lb1 ub2))
;;                   (max (* lb1 lb2) (* ub1 ub2)))
;;
;; And so on until:
;;
;; lb1 > 0 and ub1 > 0 and lb2 > 0 and ub2 > 0

;; The mapping between all the existing sign combinations and their accordinate products:
;;
;;  lb1 | ub1 | lb2 | ub2 |
;; -----|-----|-----|-----|
;;  < 0 | < 0 | < 0 | < 0 | => (make-interval (* ub1 ub2) (* lb1 lb2))
;;  < 0 | < 0 | < 0 | > 0 | => (make-interval (* lb1 ub2) (* lb1 lb2))
;;  < 0 | < 0 | > 0 | > 0 | => (make-interval (* lb1 ub2) (* ub1 lb2))
;;  < 0 | > 0 | < 0 | < 0 | => (make-interval (* ub1 lb2) (* lb1 lb2))
;;  < 0 | > 0 | < 0 | > 0 | => (make-interval (min (* ub1 lb2) (* lb1 ub2))
;;                                            (max (* lb1 lb2) (* ub1 ub2)))
;;  < 0 | > 0 | > 0 | > 0 | => (make-interval (* lb1 ub2) (* ub1 ub2))
;;  > 0 | > 0 | < 0 | < 0 | => (make-interval (* ub1 lb2) (* lb1 ub2))
;;  > 0 | > 0 | < 0 | > 0 | => (make-interval (* ub1 lb2) (* ub1 ub2))
;;  > 0 | > 0 | > 0 | > 0 | => (make-interval (* lb1 lb2) (* ub1 ub2))

;; From the mapping above, there is only one combination that requires four multiplications:
;;
;; lb1 < 0 and ub1 > 0 and lb2 < 0 and ub2 > 0
;;
;; The bounds of this combination have pairwise different signs.
;;
;; The remaining 8 combinationos require only two multiplications.
;;
;; By making the '>' signs of these 8 combinations less strict we may as well cover for
;; the case when lb1 = 0 and ub1 = 0 and lb2 = 0 and ub2 = 0, since this case
;; does not affect the result of multiplication.

;; Hence, we have the following:
;;
;; lb1 < 0  and ub1 < 0  and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 ub2) (* lb1 lb2))
;; lb1 < 0  and ub1 < 0  and lb2 < 0  and ub2 >= 0 => (make-interval (* lb1 ub2) (* lb1 lb2))
;; lb1 < 0  and ub1 < 0  and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 ub2) (* ub1 lb2))
;; lb1 < 0  and ub1 >= 0 and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 lb2) (* lb1 lb2))
;; lb1 < 0  and ub1 >= 0 and lb2 < 0  and ub2 >= 0 => (make-interval (min (* ub1 lb2) (* lb1 ub2))
;;                                                                   (max (* lb1 lb2) (* ub1 ub2)))
;; lb1 < 0  and ub1 >= 0 and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 ub2) (* ub1 ub2))
;; lb1 >= 0 and ub1 >= 0 and lb2 < 0  and ub2 < 0  => (make-interval (* ub1 lb2) (* lb1 ub2))
;; lb1 > 0  and ub1 >= 0 and lb2 < 0  and ub2 >= 0 => (make-interval (* ub1 lb2) (* ub1 ub2))
;; lb1 >= 0 and ub1 >= 0 and lb2 >= 0 and ub2 >= 0 => (make-interval (* lb1 lb2) (* ub1 ub2))
