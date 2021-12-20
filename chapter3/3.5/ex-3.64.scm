;; Exercise 3.64
;;
;; Write a procedure stream-limit that takes as arguments a stream
;; and a number (the tolerance).
;;
;; It should examine the stream until it finds two successive elements
;; that differ in absolute value by less than the tolerance, and
;; return the second of the two elements.
;;
;; Using this, we could compute square roots up to a given tolerance by

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


(load "workbook.scm")

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))


;; (sqrt 2 0.1)   ; 1.4166666666666665
;; (sqrt 2 0.01)  ; 1.4142156862745097
;; (sqrt 2 0.001) ; 1.4142135623746899
