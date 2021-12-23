;; Exercise 3.72
;;
;; In a similar way to exercise 3.71 generate a stream of all numbers
;; that can be written as the sum of two squares in three different ways
;; (showing how they can be so written).


;; To solve the task we need to:
;; 1) generate the stream of pairs of integers (i,j)
;;    weighted according to the sum i^2 + j^2
;; 2) search the stream for three consecutive pairs
;;    with the same weight

(load "workbook.scm")
(load "ex-3.70.scm")

(define (square-weight pair)
  (+ (square (car pair))
     (square (cadr pair))))

;; (square-weight (list 1 18))  ; 325
;; (square-weight (list 6 17))  ; 325
;; (square-weight (list 10 15)) ; 325
;;
;; (square-weight (list 5 20))  ; 425
;; (square-weight (list 8 19))  ; 425
;; (square-weight (list 13 16)) ; 425


(define square-weighted-pairs
  (weighted-pairs integers integers square-weight))

(define num-candidates
  (stream-map square-weight square-weighted-pairs))

;; (stream-ref num-candidates 0) ; 2
;; (stream-ref num-candidates 1) ; 5
;; (stream-ref num-candidates 2) ; 8
;; (stream-ref num-candidates 3) ; 10
;; (stream-ref num-candidates 4) ; 13
;; (stream-ref num-candidates 5) ; 17


(define (select-numbers candidates)
  (let ((x1 (stream-ref candidates 0))
        (x2 (stream-ref candidates 1))
        (x3 (stream-ref candidates 2)))
    (if (= x1 x2 x3)
        (cons-stream
         x3
         (select-numbers (stream-cdddr candidates)))
        (select-numbers (stream-cdr candidates)))))

(define sqr-numbers
  (select-numbers num-candidates))

;; (stream-ref sqr-numbers 0) ; 325
;; (stream-ref sqr-numbers 1) ; 425
;; (stream-ref sqr-numbers 2) ; 650
;; (stream-ref sqr-numbers 3) ; 725
;; (stream-ref sqr-numbers 4) ; 845
;; (stream-ref sqr-numbers 5) ; 850


;; Utils

(define (stream-cdddr s)
  (stream-cdr (stream-cdr (stream-cdr s))))
