;; Special Form: unique
;;
;; The form succeed if there is precisely one item
;; in the data base satisfying a specified query.

(load "stream-utils.scm")

(define (install-unique qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (uniquely-asserted operands frame-stream)
      (stream-flatmap
       (lambda (frame)
         (let ((qeval-stream (qeval (unique-query operands)
                                    (singleton-stream frame))))
           (if (singleton-stream? qeval-stream)
               qeval-stream
               the-empty-stream)))
       frame-stream))

    (define (unique-query exps) (car exps))

    (extend-qeval 'unique uniquely-asserted)))
