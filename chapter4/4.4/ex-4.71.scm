;; Exercise 4.71
;;
;; Louis Reasoner wonders why the simple-query and disjoin procedures
;; (section 4.4.4.2) are implemented using explicit delay operations,
;; rather than being defined as follows:

(define (simple-query query-pattern frame-stream)
  (stream-
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))

;; Can you give examples of queries where these simpler definitions
;; would lead to undesirable behavior?


(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; In the simple-query procedure, the given query is first matched
;; against the assertions, whereas the unification step is delayed
;; and only run on demand.
;;
;; That allows to populate the resulting stream with some elements
;; (each time a rule is applied) before entering the infinite loop.


;; 1. Infinite stream VS Infinite loop
;;
;; Consider the following example, where a recursively-defined rule
;; has the same name as an assertion:

(assert!
 (married Minnie Mickey))

(assert!
 (rule (married ?x ?y)
       (married ?y ?x)))


;; The following query generates an infinite stream of deductions
;; (repetitive) in case of the original simple-query and disjoin
;; procedures:

(married Mickey ?who)

;; Query results:
;;
;; (married mickey minnie)
;; (married mickey minnie)
;; (married mickey minnie)
;; ...


;; and ends up in the infinite loop searching for answers
;; without printing anything in case the proposed changes
;; to the above mentioned procedures:

(married Mickey ?who)

;; Query results:
;;
;; None


;; 2. Disjuncts
;;
;; Define a celebrity as either a rockstar or
;; one that is married to a rockstar:

(assert!
 (rule (celebrity ?x)
       (or (rockstar ?x)
           (and (rockstar ?rs)
                (married ?rs ?x)))))

(assert! (rockstar Mickey))


;; Find all celebrities (original implementation):
(celebrity ?who)

;; Query results:
;;
;; (celebrity mickey)
;; (celebrity minnie)
;; (celebrity minnie)
;; (celebrity minnie)
;; (celebrity minnie)
;; (celebrity minnie)
;; ...


;; Find all celebrities (suggested implementation):

(celebrity ?who)

;; Query results:
;;
;; None
