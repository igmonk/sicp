;; Exercise 3.14
;;
;; The following procedure is quite useful, although obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Loop uses the 'temporary' variable temp to hold the old value
;; of the cdr of x, since the set-cdr! on the next line destroys the cdr.
;;
;; Explain what mystery does in general.
;;
;; Suppose v is defined by (define v (list 'a 'b 'c 'd)).
;; Draw the box-and-pointer diagram that represents the list to which v is bound.
;; 
;; Suppose that we now evaluate (define w (mystery v)).
;; Draw box-and-pointer diagrams that show the structures v and w after
;; evaluating this expression.
;;
;; What would be printed as the values of v and w ?

(define v (list 'a 'b 'c 'd))

;; v ; (a b c d)

;; v -> |x|x|--->|x|x|--->|x|x|--->|x|/|
;;       |        |        |        |
;;       a        b        c        d


(define w (mystery v))

;; v ; (a)
;; w ; (d c b a)

;; Inside mystery v is referenced by x and its cdr is set to the empty list
;; upon the first step of the inner loop, which results in (list a):
;;
;; v -> |x|/|     temp -> |x|x|--->|x|x|--->|x|/|
;;       |                 |        |        |
;;       a                 b        c        d
;;
;; Later, v is passed as the value of the parameter y, which is not subject
;; to change anymore: none of the mutation methods are used against it.
;; Therefore, once changed to (list a), the value of v stays the same forever.
;;
;; The procedure mystery reverses a given list by setting cdr of
;; the first element of the current list to the accumulated value,
;; and using the result as an accumulated value at the next step.
;;
;; w -> |x|x|--->|x|x|--->|x|x|----↓
;;       |        |        |       |
;;       d        c        b       |
;;                                 |
;;                                 ↓
;;                           v -> |x|/|
;;                                 |
;;                                 a
