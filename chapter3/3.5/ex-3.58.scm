;; Exercise 3.58
;;
;; Give an interpretation of the stream computed by the following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Quotient is a primitive that returns the integer quotient of two integers.
;;
;; What are the successive elements produced by (expand 1 7 10) ?
;; What is produced by (expand 3 8 10) ?


;; Both streams perform long division in the decimal system and
;; generate numbers after the floating point.


;; 1. (expand 1 7 10) generates numbers after the floating point
;;                    of 1/7 = 0,1428571429

(define s1 (expand 1 7 10))

;; (stream-ref s1 0) ; 1
;; (stream-ref s1 1) ; 4
;; (stream-ref s1 2) ; 2
;; (stream-ref s1 3) ; 8
;; (stream-ref s1 4) ; 5
;; (stream-ref s1 5) ; 7

;; (stream-ref s1 6)  ; 1
;; (stream-ref s1 7)  ; 4
;; (stream-ref s1 8)  ; 2
;; (stream-ref s1 9)  ; 8
;; (stream-ref s1 10) ; 5
;; (stream-ref s1 11) ; 7

;; (stream-ref s1 12) ; 1
;; etc.


;; 2. (expand 3 8 10) generates numbers after the floating point
;;                    of 3/8 = 0,375

(define s2 (expand 3 8 10))

;; (stream-ref s2 0) ; 3
;; (stream-ref s2 1) ; 7
;; (stream-ref s2 2) ; 5
;; (stream-ref s2 3) ; 0
;; (stream-ref s2 4) ; 0
;; (stream-ref s2 5) ; 0
;; (stream-ref s2 6) ; 0
;; (stream-ref s2 7) ; 0
;; etc.

;; See: https://en.wikipedia.org/wiki/Long_division
