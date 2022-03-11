;; Exercise 4.75
;;
;; Implement for the query language a new special form called unique.
;;
;; Unique should succeed if there is precisely one item in the data base
;; satisfying a specified query. For example,

(unique (job ?x (computer wizard)))

;; should print the one-item stream

(unique (job (Bitdiddle Ben) (computer wizard)))

;; since Ben is the only computer wizard, and

(unique (job ?x (computer programmer)))

;; should print the empty stream, since there is more than
;; one computer programmer. Moreover,

(and (job ?x ?j) (unique (job ?anyone ?j)))

;; should list all the jobs that are filled by only one person,
;; and the people who fill them.

;; There are two parts to implementing unique.
;;
;; The first is to write a procedure that handles this special form,
;; and the second is to make qeval dispatch to that procedure.
;;
;; The second part is trivial, since qeval does its dispatching
;; in a data-directed way.
;;
;; If your procedure is called 'uniquely-asserted', all you need to do is

(put 'unique 'qeval uniquely-asserted)

;; and qeval will dispatch to this procedure for every query
;; whose type (car) is the symbol unique.
;;
;; The real problem is to write the procedure uniquely-asserted.
;;
;; This should take as input the contents (cdr) of the unique query,
;; together with a stream of frames. For each frame in the stream,
;; it should use qeval to find the stream of all extensions to
;; the frame that satisfy the given query.
;;
;; Any stream that does not have exactly one item in it
;; should be eliminated. The remaining streams should be passed back
;; to be accumulated into one big stream that is the result of
;; the unique query.
;;
;; This is similar to the implementation of the not special form.
;;
;; Test your implementation by forming a query that lists all people
;; who supervise precisely one person.


;; The following procedure does exactly what's required by the task:
;; it eliminates any stream that does not have exactly one item in it
;; and passes back the remaining streams to be accumulated into
;; one big stream:

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


;; An auxiliary procedure that checks whether the given stream
;; has exactly one item in it:

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))


;; Tests

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


(unique (job ?x (computer wizard)))     ; (unique (job (bitdiddle ben) (computer wizard)))
(unique (job ?x (computer programmer))) ; None

;; List all the jobs that are filled by only one person,
;; and the people who fill them:
(and (job ?x ?j) (unique (job ?anyone ?j)))

;; Query results:
;;
;; (and (job (aull dewitt) (administration secretary))
;;      (unique (job (aull dewitt) (administration secretary))))
;; (and (job (cratchet robert) (accounting scrivener))
;;      (unique (job (cratchet robert) (accounting scrivener))))
;; (and (job (scrooge eben) (accounting chief accountant))
;;      (unique (job (scrooge eben) (accounting chief accountant))))
;; (and (job (warbucks oliver) (administration big wheel))
;;      (unique (job (warbucks oliver) (administration big wheel))))
;; (and (job (reasoner louis) (computer programmer trainee))
;;      (unique (job (reasoner louis) (computer programmer trainee))))
;; (and (job (tweakit lem e) (computer technician))
;;      (unique (job (tweakit lem e) (computer technician))))
;; (and (job (bitdiddle ben) (computer wizard))
;;      (unique (job (bitdiddle ben) (computer wizard))))
