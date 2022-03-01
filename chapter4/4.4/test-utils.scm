;; Test Utilities

(load "../../chapter3/3.3/table-obj-2d.scm")

(load "data-base.scm")
(load "query-utils.scm")
(load "stream-utils.scm")
(load "frame.scm")
(load "assertion.scm")
(load "qevaluator.scm")
(load "instantiator.scm")

;; Special Forms
(load "and.scm")
(load "or.scm")
(load "not.scm")
(load "lisp-value.scm")
(load "always-true.scm")
(load "unique.scm")
(load "and-opt.scm")

;; Data base
(define db (make-data-base))
(define add-rule-or-assertion! (db 'add-rule-or-assertion!))

;; Query Evaluator
(define qevaluator (make-qevaluator db))
(define qeval (qevaluator 'qeval))

;; Query Evaluator Extensions
(install-and qevaluator)
(install-or qevaluator)
(install-not qevaluator)
(install-lisp-value qevaluator)
(install-always-true qevaluator)
(install-unique qevaluator)
(install-and-opt qevaluator)

;; Driver Loop
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))
