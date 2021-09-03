;; Exercise 2.13
;;
;; Show that under the assumption of small percentage tolerances
;; there is a simple formula for the approximate percentage tolerance
;; of the product of two intervals in terms of tolerances of the factors.
;;
;; You may simplify the problem by assuming that all numbers are positive.

(load "workbook.scm")
(load "ex-2.7.scm")
(load "ex-2.12.scm")

;; By assuming that all numbers are positive, i.e.:
;; lb1 > 0 and ub1 > 0 and lb2 > 0 and ub2 > 0,
;; the product of two intervals is an interval that has its lower and upper bounds
;; defined as follows:
;;
;; lb = lb1 * lb2
;; ub = ub1 * ub2
;;
;; where lower and upper bounds could be redefined in terms of their centers
;; and widths:
;;
;; lb = (c1 - w1) * (c2 - w2)
;; ub = (c1 + w1) * (c2 + w2)
;;
;; or, in terms of their centers and percentage tolerances:
;;
;; lb = (c1 - (c1 * pt1) / 100) * (c2 - (c2 * pt2) / 100)
;; ub = (c1 + (c1 * pt1) / 100) * (c2 + (c2 * pt2) / 100)
;;
;; lb = c1 * c2 - (c1 * c2 * pt2) / 100 - (c1 * c2 * pt1) / 100 + (c1 * c2 * pt1 * pt2) / 10000
;; ub = c1 * c2 + (c1 * c2 * pt2) / 100 + (c1 * c2 * pt1) / 100 + (c1 * c2 * pt1 * pt2) / 10000
;;
;; lb = c1 * c2 * (1 - pt2/100 - pt1/100 + pt1 * pt2 / 10000)  [1]
;; ub = c1 * c2 * (1 + pt2/100 + pt1/100 + pt1 * pt2 / 10000)  [2]
;;
;; The approximate percentage tolerance of the product is found in the following way:
;;
;; pt = 100 * w / c
;;
;; or, in terms of its lower and upper bounds:
;;
;; pt = 100 * ((ub - lb) / 2) / ((lb + ub) / 2)                [3]
;;
;; From [1] and [2] the values for 1) ub - lb and 2) lb + ub can be derived as follows:
;;
;; 1) ub - lb
;;
;; ub - lb = c1 * c2 * (1 + pt2/100 + pt1/100 + pt1 * pt2 / 10000) -
;;           c1 * c2 * (1 - pt2/100 - pt1/100 + pt1 * pt2 / 10000)
;;
;; ub - lb = c1 * c2 * (2 * pt1/100 + 2 * pt2/100)
;; ub - lb = 2 * c1 * c2 * (pt1 + pt2) / 100                   [4]
;;
;; 2) lb + ub
;;
;; lb + ub = c1 * c2 * (1 - pt2/100 - pt1/100 + pt1 * pt2 / 10000) +
;;           c1 * c2 * (1 + pt2/100 + pt1/100 + pt1 * pt2 / 10000)
;;
;; lb + ub = c1 * c2 * (2 + 2 * pt1 * pt2 / 10000)
;; lb + ub = 2 * c1 * c2 * (1 + pt1 * pt2 / 10000)             [5]
;;
;; Given [4] and [5] the approximate percentage tolerance [3] can be reformulated as:
;;
;; pt = 100 * ((2 * c1 * c2 * (pt1 + pt2) / 100) / 2) /
;;            (2 * c1 * c2 * (1 + pt1 * pt2 / 10000) / 2)
;;
;; pt = 100 * (c1 * c2 * (pt1 + pt2) / 100) / (c1 * c2 * (1 + pt1 * pt2 / 10000))
;; pt = (c1 * c2 * (pt1 + pt2)) / (c1 * c2 * (1 + pt1 * pt2 / 10000))
;; pt = (pt1 + pt2) / (1 + pt1 * pt2 / 10000)
;;
;; Hence, the approximate percentage tolerance of the product of two intervals
;; in terms of tolerances of the factors can be expressed as:
;;
;; pt = (pt1 + pt2) / (1 + pt1 * pt2 / 10000)


(define (approx-pt pt1 pt2)
  (/ (+ pt1 pt2)
     (+ 1.0 (/ (* pt1 pt2) 10000))))

;; (define i1 (make-center-percent 6.8 10))
;; (define i2 (make-center-percent 4.7 5))

;; (print-interval i1) ; [6.12,7.4799999999999995]
;; (print-interval i2) ; [4.465,4.9350000000000005]

;; (define i1*i2 (mul-interval i1 i2))

;; (print-interval i1*i2) ; [27.3258,36.9138]

;; (approx-pt 10 5) ; 14.92537313432836 (Expected)
;; (percent i1*i2)  ; 14.92537313432836 (Actual)
