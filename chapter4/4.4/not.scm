;; Special Form: not
;;
;; We attempt to extend each frame in the input stream
;; to satisfy the query being negated, and we include
;; a given frame in the output stream only if it cannot
;; be extended.

(load "stream-utils.scm")

(define (install-not qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (negate operands frame-stream)
      (stream-flatmap
       (lambda (frame)
         (if (stream-null? (qeval (negated-query operands)
                                  (singleton-stream frame)))
             (singleton-stream frame)
             the-empty-stream))
       frame-stream))

    (define (negated-query exps) (car exps))

    (extend-qeval 'not negate)))
