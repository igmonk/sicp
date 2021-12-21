;; Exercise 3.67
;;
;; Modify the pairs procedure so that (pairs integers integers)
;; will produce the stream of all pairs of integers (i,j)
;; (without the condition i < j).
;;
;; Hint: You will need to mix in an additional stream.

(load "workbook.scm")

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s)))))

;; (define int-pairs (pairs integers integers))
;;
;; (stream-ref int-pairs 0) ; (1 1)
;; (stream-ref int-pairs 1) ; (1 2)
;; (stream-ref int-pairs 2) ; (2 1)
;; (stream-ref int-pairs 3) ; (2 2)
;; (stream-ref int-pairs 4) ; (3 1)
;; (stream-ref int-pairs 5) ; (1 3)
;; (stream-ref int-pairs 6) ; (4 1)
;; (stream-ref int-pairs 7) ; (2 3)
;; (stream-ref int-pairs 8) ; (5 1)
;; (stream-ref int-pairs 9) ; (1 4)
