;; Eval xor
;;
;; Extends the given evaluator with 'xor' expressions.
;;
;; XOR is an exclusive 'or': a true output results if one,
;; and only one, of the inputs is true.
;;
;; (A and (not B)) or ((not A) and B)
;;
;; or, after the application of de Morgan's Law:
;;
;; (A or B) and (not (A and B))
;;
;; which in MIT Scheme looks as follows:
;;
;; (and (or a b) (not (and a b)))
;;
;; The form is implemented as a derived expression based on
;; the special forms 'or' and 'and'.

(define (install-eval-xor evaluator)
  (let ((_analyze (evaluator '_analyze))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))

    (define (analyze-xor exp)
      (_analyze (xor->combination exp)))

    (define (xor->combination exp)
      (let ((a (xor-a exp))
            (b (xor-b exp)))
        (make-and
         (list (make-or (list a b))
               (make-not
                (make-and (list a b)))))))

    (define (xor-a exp) (cadr exp))
    (define (xor-b exp) (caddr exp))

    ;; Dependency constructors
    (define (make-not exp) (list 'not exp))
    (define (make-or . args)
      (apply (get-constructor 'make-or) args))
    (define (make-and . args)
      (apply (get-constructor 'make-and) args))

    (extend-analyze 'xor analyze-xor)))
