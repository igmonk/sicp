;; Exercise 2.14
;;
;; After considerable work, Alyssa P. Hacker delivers her finished system.

(load "workbook.scm")
(load "ex-2.7.scm")
(load "ex-2.12.scm")

;; Several years later, after she has forgotten all about it,
;; she gets a frenzied call from an irate user, Lem E. Tweakit.
;; It seems that Lem has noticed that the formula for parallel resistors
;; can be written in two algebraically equivalent ways:
;;
;; R1 * R2 / (R1 + R2)
;;
;; and
;;
;; 1 / (1 / R1 + 1 / R2)
;;
;; He has written the following two programs, each of which
;; computes the parallel-resistors formula differently:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Lem complains that Alyssa's program gives different answers
;; for the two ways of computing. This is a serious complaint.
;;
;; Demonstrate that Lem is right.
;; Investigate the behavior of the system on a variety of arithmetic expressions.
;; Make some intervals A and B, and use them in computing the expressions A/A and A/B.
;;
;; You will get the most insight by using intervals whose width
;; is a small percentage of the center value.
;;
;; Examine the results of the computation in 'center-percent' form.


;; (define i1 (make-interval 6.12 7.48))
;; (define i2 (make-interval 4.465 4.935))

;; (print-interval i1) ; [6.12,7.48]
;; (print-interval i2) ; [4.465,4.935]

;; (define i1-par1-i2 (par1 i1 i2))
;; (define i1-par2-i2 (par2 i1 i2))

;; (print-interval i1-par1-i2) ; [2.201031010873943,3.4873689182805854]
;; (print-interval i1-par2-i2) ; [2.581558809636278,2.97332259363673]


;; The values computed with different procedures don't match.
;; par2 produces tighter error bounds than par1.
;;
;; Expected values (from the chapter 2.1.4):
;;   from about 2.58 ohms (if the two resistors are at the lower bounds)
;;   to about 2.97 ohms (if the two resistors are at the upper bounds)


;; Although, the original formulas for parallel resistors are algebraically equivalent,
;; the same cannot be said when they are applied to intervals.
;;
;; In order to derive
;;
;; R1 * R2 / (R1 + R2)    [1]
;;
;; from
;;
;; 1 / (1 / R1 + 1 / R2)  [2]
;;
;; the following steps need to be applied:
;;
;; [2] = 1 / (1 / R1 + 1 / R2)
;; [2] = 1 / ((1 / R1) * (R2 / R2) + (1 / R2) * (R1 / R1))
;; [2] = 1 / (R2 / (R1 * R2) + R1 / (R1 * R2))
;; [2] = 1 / ((R1 + R2) / R1*R2)
;; [2] = R1 * R2 / (R1 + R2)
;; [2] = [1]
;;
;; Notice that by applying algebra to make the denominators the same while
;; adding two quotiens, we implicitly multiply these quotiens by 1, represented
;; as a quotient with equal nominator and denominator values: (R1 / R1) and (R2 / R2).
;;
;; When we deal with intervals, however, it is wrong to assume that
;; (R1 / R1) = 1 and (R2 / R2) = 1 (or rather [1,1] in case of intervals),
;; unless the lower and upper bounds of the intervals are exactly the same (width = 0).


;; Below are some examples of multiplying intervals with the same lower and upper bounds.
;;
;; 1) [3,3] * [4,4]
;;
;; (define i3 (make-interval 3 3))
;; (define i4 (make-interval 4 4))
;;
;; (print-interval (par1 i3 i4)) ; [1.7142857142857142,1.7142857142857142]
;; (print-interval (par2 i3 i4)) ; [1.7142857142857144,1.7142857142857144]
;;
;;
;; 2) [3,3] * [3,3]
;;
;; (print-interval (par1 i3 i3)) ; [1.5,1.5]
;; (print-interval (par2 i3 i3)) ; [1.5,1.5]
;;
;;
;; 3) [4,4] * [4,4]
;;
;; (print-interval (par1 i4 i4)) ; [2.,2.]
;; (print-interval (par2 i4 i4)) ; [2.,2.]


;; Therefore, an equivalent of algebraic multiplication by 1 for intervals
;; is multiplication by the interval [1,1].
;;
;; (define i-one (make-interval 1 1))
;; (print-interval (mul-interval i3 i-one)) ; [3,3]
