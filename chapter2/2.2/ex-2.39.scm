;; Exercise 2.39
;;
;; Complete the following definitions of 'reverse' (exercise 2.18) in terms of
;; fold-right and fold-left from exercise 2.38:

(load "workbook.scm")
(load "ex-2.38.scm")

(define (reverse sequence)
  (fold-right (lambda (item acc)
                (append acc (list item)))
              '()
              sequence))

(define (reverse sequence)
  (fold-left (lambda (acc item)
               (cons item acc))
             '()
             sequence))

;; (reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)
