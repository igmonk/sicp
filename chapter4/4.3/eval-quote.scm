;; Eval Quote
;;
;; Extends the given amb evaluator with quote expressions.

(define (install-eval-quote evaluator)

  (define (eval-quote exp env)
    (text-of-quotation exp))

  (define (analyze-quote exp)
    (let ((qval (text-of-quotation exp)))
      (lambda (env succeed fail)
        (succeed qval fail))))

  (define (text-of-quotation exp) (cadr exp))

  ((evaluator 'extend-eval) 'quote eval-quote)
  ((evaluator 'extend-analyze) 'quote analyze-quote))
