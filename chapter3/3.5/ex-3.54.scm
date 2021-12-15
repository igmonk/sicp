;; Exercise 3.54
;;
;; Define a procedure mul-streams, analogous to add-streams,
;; that produces the elementwise product of its two input streams.

(load "workbook.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; Use this together with the stream of integers to complete
;; the following definition of the stream whose nth element
;; (counting from 0) is n + 1 factorial:

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            (stream-cdr integers))))

;; (stream-ref factorials 0) ; 1
;; (stream-ref factorials 1) ; 2
;; (stream-ref factorials 2) ; 6
;; (stream-ref factorials 3) ; 24
;; (stream-ref factorials 4) ; 120
;; (stream-ref factorials 5) ; 720
