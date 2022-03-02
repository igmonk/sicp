;; Exercise 4.55
;;
;; Give simple queries that retrieve the following information
;; from the data base:
;;
;; a. all people supervised by Ben Bitdiddle
;; b. the names and jobs of all people in the accounting division
;; c. the names and addresses of all people who live in Slumerville

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; a. all people supervised by Ben Bitdiddle
(supervisor ?x (Bitdiddle Ben))

;; Query results:
;;
;; (supervisor (tweakit lem e) (bitdiddle ben))
;; (supervisor (fect cy d) (bitdiddle ben))
;; (supervisor (hacker alyssa p) (bitdiddle ben))


;; b. the names and jobs of all people in the accounting division
(job ?x (accounting . ?type))

;; Query results:
;;
;; (job (cratchet robert) (accounting scrivener))
;; (job (scrooge eben) (accounting chief accountant))


;; c. the names and addresses of all people who live in Slumerville
(address ?x (Slumerville . ?y))

;; Query results:
;;
;; (address (aull dewitt) (slumerville (onion square) 5))
;; (address (reasoner louis) (slumerville (pine tree road) 80))
;; (address (bitdiddle ben) (slumerville (ridge road) 10))
