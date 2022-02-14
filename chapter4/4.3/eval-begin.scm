;; Eval Begin
;;
;; Extends the given evaluator with begin expressions.

(define (install-eval-begin evaluator)
  (let ((eval-sequence (evaluator '_eval-seq))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (analyze-sequence (evaluator '_analyze-seq))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-begin exp env)
      (eval-sequence (begin-actions exp) env))

    (define (analyze-begin exp)
      (analyze-sequence (begin-actions exp)))

    (define (make-begin seq) (cons 'begin seq))
    (define (begin-actions exp) (cdr exp))

    (extend-eval 'begin eval-begin)
    (extend-analyze 'begin analyze-begin)
    (def-constructor 'make-begin make-begin)))
