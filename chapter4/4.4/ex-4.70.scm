;; Exercise 4.70
;;
;; What is the purpose of the 'let' bindings in the procedures
;; add-assertion! and add-rule! ?
;;
;; What would be wrong with the following implementation of
;; add-assertion! ?
;;
;; Hint: Recall the definition of the infinite stream of ones
;;       in section 3.5.2: (define ones (cons-stream 1 ones)).

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)


;; Consider the following infinite stream defined implicitly:

(define test-stream the-empty-stream)
(set! test-stream (cons-stream 'test test-stream))

;; Which is analogous to:

(define test-stream (cons-stream 'test test-stream))


;; As soon as its first two elements, going at indexes 0 and 1,
;; have been realised, any program that try to run through
;; the whole stream (even though it is infinite) will end up
;; in the infinite loop:

test-stream ; {test ...}

(stream-ref test-stream 0) ; test
(stream-ref test-stream 1) ; test

test-stream ; Infinite loop


;; The purpose of the 'let' binding in the original procedures
;; add-assertion! and add-rule! is to avoid implicit definitions
;; of infinite streams that will inevitably lead to the infinite
;; loops when accessing THE-ASSERTIONS and THE-RULES, accordingly.
;;
;; THE-ASSERTIONS and THE-RULES are accessed when a query pattern
;; cannot be found by using index.
;;
;; Hence, in order to test the incorrect version of the procedures
;; add-assertion! and add-rule! it must be sufficient
;; (after having installed them) to insert into the database
;; at least one assertion or rule that is non-indexable and
;; try running a query that forces the query evaluator to
;; read THE-ASSERTIONS or THE-RULES, respectively.


(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)

;; Insert a non-indexable assertion to the database
(assert!
 ((non-indexable-1 ?x) ?y))

;; Try running a query that DOES NOT force to read THE-ASSERTIONS
(any-query ?x)      ; Query results: None

;; Try running a query that DOES force to read THE-ASSERTIONS
((any-query ?x) ?y) ; Aborting!: maximum recursion depth exceeded
