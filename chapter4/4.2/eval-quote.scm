;; Eval Quote
;;
;; Extends the given evaluator with quote expressions.

(define (install-eval-quote evaluator)

  (define (eval-quote exp env)
    (let ((quoted (text-of-quotation exp)))
      (if (pair? quoted)
          ((evaluator '_eval) (make-list quoted) env)
          quoted)))

  (define (analyze-quote exp)
    (let ((qval (text-of-quotation exp)))
      (lambda (env) qval)))

  (define (text-of-quotation exp) (cadr exp))

  (define (make-list exps)
    (if (null? exps)
        '()
        (list 'cons
              (car exps)
              (make-list (cdr exps)))))

  ((evaluator 'extend-eval) 'quote eval-quote)
  ((evaluator 'extend-analyze) 'quote analyze-quote))
