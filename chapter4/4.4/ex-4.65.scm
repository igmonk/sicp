;; Exercise 4.65
;;
;; Cy D. Fect, looking forward to the day when he will rise
;; in the organization, gives a query to find all the wheels
;; (using the wheel rule of section 4.4.1):

(wheel ?who)

;; To his surprise, the system responds
;;
;; Query results:
;;
;; (wheel (warbucks oliver))
;; (wheel (warbucks oliver))
;; (wheel (bitdiddle ben))
;; (wheel (warbucks oliver))
;; (wheel (warbucks oliver))

;; Why is Oliver Warbucks listed four times?


(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


(assert!
 (rule (wheel ?person)
       (and (supervisor ?middle-manager ?person)
            (supervisor ?x ?middle-manager))))


;; Warbucks Oliver is the supervisor of
;; - Bitdiddle Ben, who, in turn, is the supervisor of
;;   - Hacker Alyssa P
;;   - Fect Cy D
;;   - Tweakit Lem E
;; - Scrooge Eben, who, in turn, is the supervisor of Cratchet Robert
;;
;; Bitdiddle Ben is the supervisor of
;; - Hacker Alyssa P, who, in turn, is the supervisor of
;;   - Reasoner Louis
;;
;; The body of the rule, after extending the initial frame by
;; binding ?person to Warbucks Oliver and ?middle-manager to
;; Bitdiddle Ben, goes on to find those whose supervisor is
;; Bitdiddle Ben. The result is three frames, each binding ?x to
;; one of: Hacker Alyssa P, Fect Cy D and Tweakit Lem E.
;;
;; In addition, after extending the initial frame by binding ?person
;; to Warbucks Oliver and ?middle-manager to Scrooge Eben, the body
;; of the rule goes on to find those whose supervisor is Scrooge Eben.
;; The result is one frame where ?x is bound to Cratchet Robert.
;;
;; Finally, when the initial frame is extended by binding ?person
;; to Bitdiddle Ben and ?middle-manager to Hacker Alyssa P,
;; the rule body keeps on to find those whose supervisor is
;; Hacker Alyssa P. The result is one frame where ?x is bound to
;; Reasoner Louis.
;;
;; After combining (appending) the intermediate streams of frames,
;; the final stream will consist of 5 elements, 1 of which is
;; Bitdiddle Ben and another 4 are related to Warbucks Oliver.
;;
;; Hence, Warbucks Oliver is listed 4 times even though there are
;; only 2 middle managers of whom Warbucks Oliver is the supervisor.
