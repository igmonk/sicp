;; Exercise 2.33
;;
;; Fill in the missing expressions to complete the following definitions
;; of some basic list-manipulation operations as accumulations:

(load "workbook.scm")


(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

;; (map (lambda (x) (* 2 x)) (list 1 2 3 4 5)) ; (2 4 6 8 10)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; (append (list 1 2 3 4) (list 5 6 7 8))


(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; (length '())            ; 0
;; (length (list 1))       ; 1
;; (length (list 1 2 3 4)) ; 4
