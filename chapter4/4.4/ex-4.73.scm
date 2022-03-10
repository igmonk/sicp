;; Exercise 4.73
;;
;; Why does flatten-stream use delay explicitly?
;; What would be wrong with defining it as follows:

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))


;; flatten-stream uses delay explicitly to avoid the infinite loop.
;;
;; Due to the Scheme's applicative order, prior to invoking
;; the interleave procedure, both its arguments need to be
;; evaluated, of which the second is calculated by
;; the recursive call to the flatten-stream procedure.
;;
;; In case of an intifite stream, such a forcing leads to
;; the infinite loop without printing out anything.


;; In addition, install the interleave procedure (section 3.5.3).

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; Given the 'ones' and 'sevens' assertions and rules
;; defined in ex. 4.72

(assert! (ones ()))

(assert!
 (rule (ones (1 . ?x))
       (ones ?x)))

(assert! (sevens ()))

(assert!
 (rule (sevens (7 . ?x))
       (sevens ?x)))

;; we can run a compound query to simulate the simplest chain of
;; flatten-stream invocations with infinite streams, where
;; the output of the first one becomes the input of another one.
;;
;; Such a chain can be created via 'and' and ends up in
;; the infinite loop without giving out any results if
;; the suggested interleave procedure is used.
;;
;; Test: interleave without delay
(and (ones ?x) (sevens ?y)) ;; ... Infinite loop


;; The original interleave-delayed is devoid of that shortcoming.
;;
;; Test: interleave with delay
(and (ones ?x) (sevens ?y))

;;; Query results:
(and (ones ()) (sevens ()))
(and (ones (1)) (sevens ()))
(and (ones ()) (sevens (7)))
(and (ones (1 1)) (sevens ()))
(and (ones ()) (sevens (7 7)))
(and (ones (1)) (sevens (7)))
(and (ones ()) (sevens (7 7 7)))
(and (ones (1 1 1)) (sevens ()))
(and (ones ()) (sevens (7 7 7 7)))
(and (ones (1)) (sevens (7 7)))
(and (ones ()) (sevens (7 7 7 7 7)))
(and (ones (1 1)) (sevens (7)))
(and (ones ()) (sevens (7 7 7 7 7 7)))
(and (ones (1)) (sevens (7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7)))
(and (ones (1 1 1 1)) (sevens ()))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7)))
(and (ones (1)) (sevens (7 7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7)))
(and (ones (1 1)) (sevens (7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7)))
(and (ones (1)) (sevens (7 7 7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7 7)))
(and (ones (1 1 1)) (sevens (7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7 7 7)))
(and (ones (1)) (sevens (7 7 7 7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7)))
(and (ones (1 1)) (sevens (7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7 7)))
(and (ones (1)) (sevens (7 7 7 7 7 7 7)))
(and (ones ()) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)))
