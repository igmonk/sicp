;; Exercise 3.68
;;
;; Louis Reasoner thinks that building a stream of pairs from
;; three parts is unnecessarily complicated.
;;
;; Instead of separating the pair (S0,T0) from the rest of the pairs
;; in the first row, he proposes to work with the whole first row,
;; as follows:
;;
;; (define (pairs s t)
;;   (interleave
;;    (stream-map (lambda (x) (list (stream-car s) x))
;;                t)
;;    (pairs (stream-cdr s) (stream-cdr t))))


;; The suggested definition triggers the infinite recursion
;; due to the lack of delayed evaluation (the recursive call
;; gets evaluated being the 2nd argument of interleave),
;; which was part of what the original version
;; of the procedure 'pairs' returns:
;;
;; (cons-stream
;;  (list (stream-car s) (stream-car t))
;;  <the-rest-is-delayed>

;; An attempt to invoke Louis's procedure ends up as follows:
;;
;; (define int-pairs (pairs integers integers))
;;
;; ;Aborting!: maximum recursion depth exceeded
