;; Exercise 2.9
;;
;; The width of an interval is half of the difference between its upper and lower bounds.
;; The width is a measure of the uncertainty of the number specified by the interval.
;;
;; For some arithmetic operations the width of the result of combining two intervals is
;; a function only of the widths of the argument intervals, whereas for others the width
;; of the combination is not a function of the widths of the argument intervals.
;;
;; Show that the width of the sum (or difference) of two intervals is a function only of
;; the widths of the intervals being added (or subtracted).
;; Give examples to show that this is not true for multiplication or division.

(load "workbook.scm")
(load "ex-2.7.scm")
(load "ex-2.8.scm")

(define i1 (make-interval 1 5))
(define i2 (make-interval 2 8))


;; 1. The width of the sum of two intervals is a function only of the widths
;;    of the intervals being added.
;;
;; i = i1 + i2
;; i = [lb1 + lb2, ub1 + ub2]
;; i = [lb1 + lb2, lb1 + 2w1 + lb2 + 2w2]
;;
;; width(i) = abs(ub(i) - lb(i)) / 2
;; width(i) = abs(lb1 + 2w1 + lb2 + 2w2 - (lb1 + lb2)) / 2
;; width(i) = abs(2w1 + 2w2) / 2
;; width(i) = abs(w1 + w2)
;; width(i) = w1 + w2
;;
;; Hence, the width of the sum of two intervals is the sum of the widths
;; of the intervals being added.

(define i1+i2 (add-interval i1 i2))

;; (print-interval i1+i2) ; [3,13]
;; (interval-width i1+i2) ; 5

;; (= (interval-width i1+i2)
;;    (+ (interval-width i1) (interval-width i2))) ; true


;; 2. The width of the difference of two intervals is a function only of the widths
;;    of the intervals being subtracted.
;;
;; i = i1 - i2
;; i = [lb1 - ub2, ub1 - lb2]
;; i = [lb1 - lb2 - 2w2, lb1 + 2w1 - lb2]
;;
;; width(i) = abs(ub(i) - lb(i)) / 2
;; width(i) = abs(lb1 + 2w1 - lb2 - (lb1 - lb2 - 2w2)) / 2
;; width(i) = abs(2w1 + 2w2) / 2
;; width(i) = abs(w1 + w2)
;; width(i) = w1 + w2
;;
;; Hence, the width of the difference of two intervals is the sum of the widths
;; of the intervals being subtracted.

(define i1-i2 (sub-interval i1 i2))

;; (print-interval i1-i2) ; [-7,3]
;; (interval-width i1-i2) ; 5

;; (= (interval-width i1-i2)
;;    (+ (interval-width i1) (interval-width i2))) ; true


;; 3. The width of the product (or quotient) of two intervals is a function of not only of
;;    the widths of the intervals being multiplied (or divided).
;;
;; For a simplified case, when the lower and upper bounds of two intervals being multiplied
;; are positive numbers, we could derive the width of the product in the following way:
;;
;; i = i1 * i2
;; i = [lb1 * lb2, ub1 * ub2]
;; i = [lb1 * lb2, (lb1 + 2w1) * (lb2 + 2w2)]
;; i = [lb1 * lb2, lb1 * lb2 + 2 * lb1 * w2 + 2 * lb2 * w1 + 4 * w1 * w2]
;;
;; width(i) = abs(ub(i) - lb(i)) / 2
;; width(i) = abs(lb1 * lb2 + 2 * lb1 * w2 + 2 * lb2 * w1 + 4 * w1 * w2 - (lb1 * lb2)) / 2
;; width(i) = abs(2 * lb1 * w2 + 2 * lb2 * w1 + 4 * w1 * w2) / 2
;; width(i) = abs(lb1 * w2 + lb2 * w1 + 2 * w1 * w2)
;; width(i) = lb1 * w2 + lb2 * w1 + 2 * w1 * w2
;;
;; It shows that even when the lower and upper bounds are positive numbers,
;; the width of the product of two intervals depends not only on the widths
;; of the intervals being multiplied.
;;
;; In general, however, we can't predict the product of what bounds will be
;; the lower (or upper) bound of the product of two intervals, since some bounds
;; could be negative numbers.
;;
;; That's why it's important to find all possible products and apply the min and max
;; procedures to figure out the resulting lower and upper bound.
;;
;; Hence, the width of the product (or quotient) of two intervals is a function of not only
;; the widths of the intervals being multiplied (or divided).

(define i1*i2 (mul-interval i1 i2))

;; (print-interval i1*i2) ; [2,40]
;; (interval-width i1*i2) ; 19

;; (= (interval-width i1*i2)
;;    (abs (+ (* (lower-bound i1) (interval-width i2))
;;            (* (lower-bound i2) (interval-width i1))
;;            (* 2 (interval-width i1) (interval-width i2))))) ; true

(define i3 (make-interval -5 -1))
(define i1*i3 (mul-interval i1 i3))

;; (print-interval i1*i3) ; [-25,-1]
;; (interval-width i1*i3) ; 12

;; (= (interval-width i1*i3)
;;    (abs (+ (* (lower-bound i1) (interval-width i3))
;;            (* (lower-bound i3) (interval-width i1))
;;            (* 2 (interval-width i1) (interval-width i3))))) ; false
