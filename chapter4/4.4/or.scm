;; Special Form: or
;;
;; The 'or' combination of two queries is produced by
;; operating on the stream of frames in parallel and
;; merging the results.

(load "stream-utils.scm")

(define (install-or qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (disjoin disjuncts frame-stream)
      (if (empty-disjunction? disjuncts)
          the-empty-stream
          (interleave-delayed
           (qeval (first-disjunct disjuncts) frame-stream)
           (delay (disjoin (rest-disjuncts disjuncts)
                           frame-stream)))))

    (define (empty-disjunction? exps) (null? exps))
    (define (first-disjunct exps) (car exps))
    (define (rest-disjuncts exps) (cdr exps))

    (extend-qeval 'or disjoin)))
