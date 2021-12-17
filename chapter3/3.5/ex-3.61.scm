;; Exercise 3.61
;;
;; Let S be a power series (exercise 3.59) whose constant term is 1.
;;
;; Suppose we want to find the power series 1/S, that is, the series X
;; such that S * X = 1.
;;
;; Write S = 1 + S(R) where S(R) is the part of S after the constant term.
;; Then we can solve for X as follows:
;;
;;          S * X = 1
;; (1 + S(R)) * X = 1
;;   X + S(R) * X = 1
;;              X = 1 - S(R) * X
;;
;; In other words, X is the power series whose constant term is 1 and
;; whose higher-order terms are given by the negative of S(R) times X.
;;
;; Use this idea to write a procedure invert-unit-series that computes
;; 1/S for a power series S with constant term 1.
;; 
;; You will need to use mul-series from exercise 3.60.


(load "workbook.scm")
(load "ex-3.60.scm")

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream
                (mul-series (stream-cdr s)
                            (invert-unit-series s))
                -1)))


;; Tests
;;
;; (define integers-uinv (invert-unit-series integers))
;;
;; (define int-*-int-uinv
;;   (mul-series integers integers-uinv))
;;
;; (stream-ref int-*-int-uinv 0) ; 1
;; (stream-ref int-*-int-uinv 1) ; 0
;; (stream-ref int-*-int-uinv 2) ; 0
;; (stream-ref int-*-int-uinv 3) ; 0
;; (stream-ref int-*-int-uinv 4) ; 0
;; (stream-ref int-*-int-uinv 5) ; 0, etc.
