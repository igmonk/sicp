;; Exercise 4.72
;;
;; Why do disjoin and stream-flatmap interleave the streams
;; rather than simply append them?
;;
;; Give examples that illustrate why interleaving works better.
;; (Hint: Why did we use interleave in section 3.5.3?)

(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)


;; As it was stated in section 3.5.3, a simple append is unsuitable
;; for infinite streams because it takes all the elements from
;; the first stream before incorporating the second stream.


(assert! (ones ()))

(assert!
 (rule (ones (1 . ?x))
       (ones ?x)))

;; Generate the stream of increasing lists of ones
(ones ?x)

;; Query results:
;;
;; (ones ())
;; (ones (1))
;; (ones (1 1))
;; (ones (1 1 1))
;; (ones (1 1 1 1))
;; (ones (1 1 1 1 1))


(assert! (sevens ()))

(assert!
 (rule (sevens (7 . ?x))
       (sevens ?x)))

;; Generate the stream of increasing lists of sevens:
(sevens ?x)

;; Query results:
;;
;; (sevens ())
;; (sevens (7))
;; (sevens (7 7))
;; (sevens (7 7 7))
;; (sevens (7 7 7 7))
;; (sevens (7 7 7 7 7))


;; Test: interleave-delayed
;;
;; Generate the stream of alternating increasing lists
;; of ones and sevens:

(or (ones ?x) (sevens ?y))

;; Query results:
;;
;; (or (ones ()) (sevens ?y))
;; (or (ones ?x) (sevens ()))
;; (or (ones (1)) (sevens ?y))
;; (or (ones ?x) (sevens (7)))
;; (or (ones (1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7)))
;; (or (ones (1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7)))
;; (or (ones (1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7)))
;; (or (ones (1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones ?x) (sevens (7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))


;; Test: stream-append-delayed
;;
;; Generate the stream of alternating increasing lists
;; of ones and sevens:

(or (ones ?x) (sevens ?y))

;; Query results:
;;
;; (or (ones ()) (sevens ?y))
;; (or (ones (1)) (sevens ?y))
;; (or (ones (1 1)) (sevens ?y))
;; (or (ones (1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
;; (or (ones (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) (sevens ?y))
