;; Special Form: and-opt
;;
;; The 'and' combination of two queries is produced by
;; processing the clauses separately and then looking for
;; all pairs of output frames that are compatible.
;;
;; See: exercise 4.76

(load "stream-utils.scm")
(load "unifier.scm")

(define (install-and-opt qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (conjoin conjuncts frame-stream)
      (if (empty-conjunction? conjuncts)
          frame-stream
          (merge-frame-streams
           (qeval (first-conjunct conjuncts) frame-stream)
           (conjoin (rest-conjuncts conjuncts) frame-stream))))

    (define (merge-frame-streams s1 s2)
      (stream-flatmap
       (lambda (frame1)
         (stream-filter
          (lambda (merged) (not (eq? merged 'failed)))
          (stream-map
           (lambda (frame2)
             (merge-frames frame1 frame2))
           s2)))
       s1))

    (define (merge-frames f1 f2)
      (if (null? f1)
          f2
          (let ((var (caar f1))
                (val (cdar f1)))
            (let ((extended (extend-if-possible var val f2)))
              (if (eq? extended 'failed)
                  'failed
                  (merge-frames (cdr f1) extended))))))
    
    (define (empty-conjunction? exps) (null? exps))
    (define (first-conjunct exps) (car exps))
    (define (rest-conjuncts exps) (cdr exps))

    (extend-qeval 'and-opt conjoin)))
