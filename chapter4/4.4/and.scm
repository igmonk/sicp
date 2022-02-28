;; Special Form: and
;;
;; The 'and' combination of two queries is produced by
;; operating on the stream of frames in series.

(define (install-and qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (conjoin conjuncts frame-stream)
      (if (empty-conjunction? conjuncts)
          frame-stream
          (conjoin (rest-conjuncts conjuncts)
                   (qeval (first-conjunct conjuncts)
                          frame-stream))))

    (define (empty-conjunction? exps) (null? exps))
    (define (first-conjunct exps) (car exps))
    (define (rest-conjuncts exps) (cdr exps))

    (extend-qeval 'and conjoin)))
