;; Special Form: always-true
;;
;; The form provides for a query that is always satisfied.
;;
;; It ignores its contents, which is normally empty, and
;; simply passes through all the frames in the input stream.
;;
;; Is used by the 'rule-body' selector to provide bodies
;; for rules that were defined without bodies (rules
;; whose conclusions are always satisfied).

(define (install-always-true qevaluator)
  (let ((extend-qeval (qevaluator 'extend-qeval)))

    (define (always-true ignore frame-stream) frame-stream)

    (extend-qeval 'always-true always-true)))
