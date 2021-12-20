;; Exercise 3.65
;;
;; Use the series
;;
;; ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...
;;
;; to compute three sequences of approximations to the
;; natural logarithm of 2, in the same way we did above for pi.
;;
;; How rapidly do these sequences converge?

(load "workbook.scm")


;; 1. Plain approximation to ln2

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; (stream-ref ln2-stream 0) ; 1.
;; (stream-ref ln2-stream 1) ; .5
;; (stream-ref ln2-stream 2) ; .8333333333333333
;; (stream-ref ln2-stream 3) ; .5833333333333333
;; (stream-ref ln2-stream 4) ; .7833333333333332
;; (stream-ref ln2-stream 5) ; .6166666666666666
;; (stream-ref ln2-stream 6) ; .7595238095238095
;; (stream-ref ln2-stream 7) ; .6345238095238095


;; 2. Accelerated approximation to ln2 (Euler's technique)

(define euler-ln2-stream
  (euler-transform ln2-stream))

;; (stream-ref euler-ln2-stream 0) ; .7
;; (stream-ref euler-ln2-stream 1) ; .6904761904761905
;; (stream-ref euler-ln2-stream 2) ; .6944444444444444
;; (stream-ref euler-ln2-stream 3) ; .6924242424242424
;; (stream-ref euler-ln2-stream 4) ; .6935897435897436
;; (stream-ref euler-ln2-stream 5) ; .6928571428571428
;; (stream-ref euler-ln2-stream 6) ; .6933473389355742
;; (stream-ref euler-ln2-stream 7) ; .6930033416875522


;; 3. Accelerated approximation to ln2 (tableau)

(define accel-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))

;; (stream-ref accel-ln2-stream 0) ; 1.
;; (stream-ref accel-ln2-stream 1) ; .7
;; (stream-ref accel-ln2-stream 2) ; .6932773109243697
;; (stream-ref accel-ln2-stream 3) ; .6931488693329254
;; (stream-ref accel-ln2-stream 4) ; .6931471960735491
;; (stream-ref accel-ln2-stream 5) ; .6931471806635636
;; (stream-ref accel-ln2-stream 6) ; .6931471805604039
;; (stream-ref accel-ln2-stream 7) ; .6931471805599445


;; ln(2) = 0,6931471806...
