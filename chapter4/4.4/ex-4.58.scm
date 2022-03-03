;; Exercise 4.58
;;
;; Define a rule that says that a person is a "big shot" in a division
;; if the person works in the division but does not have a supervisor
;; who works in the division.

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; Import the 'same' rule
(assert!
 (rule (same ?x ?x)))


;; The 'big-shot' rule
(assert!
 (rule (big-shot ?person ?division)
       (and (job ?person (?division . ?whatever-1))
            (not (and (job ?boss (?division . ?whatever-2))
                      (supervisor ?person ?boss)
                      (not (same ?person ?boss)))))))


(big-shot ?person ?division)

;; Query results:
;;
;; (big-shot (scrooge eben) accounting)
;; (big-shot (warbucks oliver) administration)
;; (big-shot (bitdiddle ben) computer)
