;; Exercise 4.1
;;
;; Notice that we cannot tell whether the metacircular evaluator
;; evaluates operands from left to right or from right to left.
;;
;; Its evaluation order is inherited from the underlying Lisp:
;;   If the arguments to cons in list-of-values are evaluated
;;   from left to right, then list-of-values will evaluate operands
;;   from left to right; and
;;   if the arguments to cons are evaluated from right to left,
;;   then list-of-values will evaluate operands from right to left.
;;
;; Write a version of list-of-values that evaluates operands
;; from left to right regardless of the order of evaluation
;; in the underlying Lisp.
;;
;; Also write a version of list-of-values that evaluates operands
;; from right to left.


;; The original procedure list-of-values:

;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (cons (eval (first-operand exps) env)
;;             (list-of-values (rest-operands exps) env))))


;; The structure of the procedure list-of-values is reminiscent of
;; a simplified version of it:

(define (run-list f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (run-list f (cdr l)))))

;; The recursive nature of it does not guarantee the evaluation order
;; of the arguments to 'cons', and is up to a concrete language
;; implementation.


;; Given the following procedure, we can trace the actual order
;; the arguments to 'cons' in 'run-list' are evaluated:

(define (display-and-return x)
  (newline)
  (display x)
  x)

;; (run-list display-and-return '(1 2 3 4 5))
;;
;; 5
;; 4
;; 3
;; 2
;; 1
;; ;Value: (1 2 3 4 5)


;; As shown above, the implementation-specific order of evaluation
;; is right-to-left.


;; By making use of the iterative approach, where, at each step,
;; an iterative call to itself guarantees the evaluation of the
;; only argument pertained to the given list, the evaluation order
;; begins to be controlled.


;; Left-to-right

(define (run-list-left-to-right f l)
  (define (inner x acc)
    (if (null? x)
        acc
        (inner (cdr x) (append acc (list (f (car x)))))))
  (inner l '()))

;; (run-list-left-to-right display-and-return
;;                         '(1 2 3 4 5))
;;
;; 1
;; 2
;; 3
;; 4
;; 5
;; ;Value: (1 2 3 4 5)


;; Right-to-left (based on reverse and left-to-right)

(define (run-list-right-to-left f l)
  (reverse
   (run-list-left-to-right f (reverse l))))

;; (run-list-right-to-left display-and-return
;;                         '(1 2 3 4 5))
;;
;; 5
;; 4
;; 3
;; 2
;; 1
;; ;Value: (1 2 3 4 5)


;; An alternative approach would be to use 'let' (nested).


;; Left-to-right

(define (run-list-l-to-r f l)
  (if (null? l)
      '()
      (let ((left (f (car l))))
        (let ((right (run-list-l-to-r f (cdr l))))
          (cons left right)))))

;; (run-list-l-to-r display-and-return '(1 2 3 4 5))
;;
;; 1
;; 2
;; 3
;; 4
;; 5
;; ;Value: (1 2 3 4 5)


;; Right-to-left

(define (run-list-r-to-l f l)
  (if (null? l)
      '()
      (let ((right (run-list-r-to-l f (cdr l))))
        (let ((left (f (car l))))
          (cons left right)))))

;; (run-list-r-to-l display-and-return '(1 2 3 4 5))
;;
;; 5
;; 4
;; 3
;; 2
;; 1
;; ;Value: (1 2 3 4 5)
