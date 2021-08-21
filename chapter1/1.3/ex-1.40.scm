;; Exercise 1.40
;;
;; Define a procedure 'cubic' that can be used together with the newtons-method procedure
;; in expressions of the form
;;
;; (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c

(load "workbook.scm")

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) c)))

;; (newtons-method (cubic 1 2 3) 1) ; -1.8637065278186378
;; (newtons-method (cubic 3 5 7) 1) ; -3.5541492186007666
