;; Exercise 4.76
;;
;; Our implementation of 'and' as a series combination of queries
;; (figure 4.5) is elegant, but it is inefficient because in
;; processing the second query of the 'and' we must scan
;; the data base for each frame produced by the first query.
;;
;; If the data base has N elements, and a typical query produces
;; a number of output frames proportional to N (say N/k), then
;; scanning the data base for each frame produced by the first query
;; will require N^2/k calls to the pattern matcher.
;;
;; Another approach would be to process the two clauses of the 'and'
;; separately, then look for all pairs of output frames that
;; are compatible. If each query produces N/k output frames, then
;; this means that we must perform N^2/k^2 compatibility checks -
;; a factor of k fewer than the number of matches required in our
;; current method.


;; Start with the procedure conjoin, which will produce recursive
;; process (previously iterative).
;;
;; The procedure merges frame streams produced by separate
;; evaluations of the given conjuncts in relation to the
;; original frame stream.

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-streams
       (qeval (first-conjunct conjuncts) frame-stream)
       (conjoin (rest-conjuncts conjuncts) frame-stream))))

;; To merge two frame streams, each frame of the first one
;; is merged with each frame of the second one, producing
;; a stream of streams with failed merged excluded, which
;; is then passed to stream-flatmap to combine the resulting
;; stream of frames:

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


;; The merge of two frames f1 and f2 is the merge of
;; (cdr f1) and the extension of f2 with the var and val
;; of (car f1).

(define (merge-frames f1 f2)
  (if (null? f1)
      f2
      (let ((var (caar f1))
            (val (cdar f1)))
        (let ((extended (extend-if-possible var val f2)))
          (if (eq? extended 'failed)
              'failed
              (merge-frames (cdr f1) extended))))))


;; See: and-opt.scm


;; Tests

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; 1. Find all people who can do the job of a computer programmer trainee:
(and-opt (can-do-job ?x (computer programmer trainee))
         (job ?person ?x))

;; Query results:
;;
;; (and-opt (can-do-job (computer programmer) (computer programmer trainee))
;;          (job (fect cy d) (computer programmer)))
;; (and-opt (can-do-job (computer programmer) (computer programmer trainee))
;;          (job (hacker alyssa p) (computer programmer)))


;; 2. Find all supervisors whose job is not computer programmer:
(and-opt (supervisor ?x ?y)
         (not (job ?x (computer programmer))))

;; Query results:
;;
;; None.

;; This happens due to the fact the conjuncts of 'and-opt'
;; are evaluated in parallel, which, when such constructs
;; as 'not' and 'lisp-value' are involved, can produce
;; empty streams if some of the variables are unbound.


;; This issue is not observed when using 'and' with a proper
;; order of conjuncts, i.e. the 'not' and 'lisp-value' special
;; forms get evaluated only when all the necessary variables
;; are bound:

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

;; Query results:
;;
;; (and (supervisor (aull dewitt) (warbucks oliver))
;;      (not (job (aull dewitt) (computer programmer))))
;; (and (supervisor (cratchet robert) (scrooge eben))
;;      (not (job (cratchet robert) (computer programmer))))
;; (and (supervisor (scrooge eben) (warbucks oliver))
;;      (not (job (scrooge eben) (computer programmer))))
;; (and (supervisor (bitdiddle ben) (warbucks oliver))
;;      (not (job (bitdiddle ben) (computer programmer))))
;; (and (supervisor (reasoner louis) (hacker alyssa p))
;;      (not (job (reasoner louis) (computer programmer))))
;; (and (supervisor (tweakit lem e) (bitdiddle ben))
;;      (not (job (tweakit lem e) (computer programmer))))
