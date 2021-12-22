;; Exercise 3.69
;;
;; Write a procedure 'triples' that takes three infinite streams,
;; S, T, and U, and produces the stream of triples (Si,Tj,Uk)
;; such that i <= j <= k.
;;
;; Use 'triples' to generate the stream of all Pythagorean triples
;; of positive integers, i.e., the triples (i,j,k)
;; such that i <= j and i^2 + j^2 = k^2.

(load "workbook.scm")

;; For each item of the first stream s,
;; enumerate all the pairs made up of the other streams t and u,
;; and for each of them prepend the item to form a triple.

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (tu-pair)
                  (cons (stream-car s) tu-pair))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define int-triples (triples integers integers integers))

;; (stream-ref int-triples 0)  ; (1 1 1)
;; (stream-ref int-triples 1)  ; (1 1 2)
;; (stream-ref int-triples 2)  ; (2 2 2)
;; (stream-ref int-triples 3)  ; (1 2 2)
;; (stream-ref int-triples 4)  ; (2 2 3)
;; (stream-ref int-triples 5)  ; (1 1 3)
;; (stream-ref int-triples 6)  ; (3 3 3)
;; (stream-ref int-triples 7)  ; (1 2 3)
;; (stream-ref int-triples 8)  ; (2 3 3)
;; (stream-ref int-triples 9)  ; (1 1 4)
;; (stream-ref int-triples 10) ; (3 3 4)
;; (stream-ref int-triples 11) ; (1 3 3)
;; (stream-ref int-triples 12) ; (2 2 4)
;; (stream-ref int-triples 13) ; (1 1 5)
;; (stream-ref int-triples 14) ; (4 4 4)
;; (stream-ref int-triples 15) ; (1 2 4)
;; (stream-ref int-triples 16) ; (2 3 4)


(define (pythagorean? a b c)
  (= (+ (square a) (square b))
     (square c)))

;; (pythagorean? 1 2 3)  ; false
;; (pythagorean? 3 4 5)  ; true
;; (pythagorean? 6 8 10) ; true


(define pythagorean-triples
  (stream-filter
   (lambda (x)
     (pythagorean? (car x) (cadr x) (caddr x)))
   int-triples))

;; (stream-ref pythagorean-triples 0) ; (3 4 5)
;; (stream-ref pythagorean-triples 1) ; (6 8 10)
;; (stream-ref pythagorean-triples 2) ; (5 12 13)
;; (stream-ref pythagorean-triples 3) ; (9 12 15)
;; (stream-ref pythagorean-triples 4) ; (8 15 17)
