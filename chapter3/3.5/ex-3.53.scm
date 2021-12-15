;; Exercise 3.53
;;
;; Without running the program, describe the elements of the stream
;; defined by

(load "workbook.scm")

(define s (cons-stream 1 (add-streams s s)))

;; Each subsequent element is twice the size of the previous one.
;;
;; The first element is 1, which is followed by
;; the sum of 1 and 1 = 2, which is followed by
;; the sum of 2 and 2 = 4, which is followed by
;; the sum of 4 and 4 = 8, which is followed by
;; the sum of 8 and 8 = 16, and so on.
;;
;; Hence, there is an alternative way of generating
;; the powers of 2.

;; Tests:
;;
;; (stream-ref s 0) ; 1
;; (stream-ref s 1) ; 2
;; (stream-ref s 2) ; 4
;; (stream-ref s 3) ; 8
;; (stream-ref s 4) ; 16
;; (stream-ref s 5) ; 32
;; (stream-ref s 6) ; 64
;; (stream-ref s 7) ; 128
;; (stream-ref s 8) ; 256
