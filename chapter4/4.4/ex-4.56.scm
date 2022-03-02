;; Exercise 4.56
;;
;; Formulate compound queries that retrieve the following information:
;;
;; a. the names of all people who are supervised by
;;    Ben Bitdiddle, together with their addresses
;; b. all people whose salary is less than Ben Bitdiddle's,
;;    together with their salary and Ben Bitdiddle's salary
;; c. all people who are supervised by someone who is not in
;;    the computer division, together with the supervisor's
;;    name and job

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; a. the names of all people who are supervised by
;;    Ben Bitdiddle, together with their addresses
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

;; Query results:
;;
;; (and (supervisor (tweakit lem e) (bitdiddle ben))
;;      (address (tweakit lem e) (boston (bay state road) 22)))
;; (and (supervisor (fect cy d) (bitdiddle ben))
;;      (address (fect cy d) (cambridge (ames street) 3)))
;; (and (supervisor (hacker alyssa p) (bitdiddle ben))
;;      (address (hacker alyssa p) (cambridge (mass ave) 78)))


;; b. all people whose salary is less than Ben Bitdiddle's,
;;    together with their salary and Ben Bitdiddle's salary
(and (salary ?person ?salary)
     (salary (Bitdiddle Ben) ?salary-bb)
     (lisp-value < ?salary ?salary-bb))

;; Query results:
;;
;; (and (salary (aull dewitt) 25000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 25000 60000))
;; (and (salary (cratchet robert) 18000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 18000 60000))
;; (and (salary (reasoner louis) 30000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 30000 60000))
;; (and (salary (tweakit lem e) 25000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 25000 60000))
;; (and (salary (fect cy d) 35000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 35000 60000))
;; (and (salary (hacker alyssa p) 40000)
;;      (salary (bitdiddle ben) 60000)
;;      (lisp-value < 40000 60000))


;; c. all people who are supervised by someone who is not in
;;    the computer division, together with the supervisor's
;;    name and job
(and (supervisor ?person ?boss)
     (not (job ?boss (computer . ?type)))
     (job ?boss ?x))

;; Query results:
;;
;; (and (supervisor (aull dewitt) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?type)))
;;      (job (warbucks oliver) (administration big wheel)))
;; (and (supervisor (cratchet robert) (scrooge eben))
;;      (not (job (scrooge eben) (computer . ?type)))
;;      (job (scrooge eben) (accounting chief accountant)))
;; (and (supervisor (scrooge eben) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?type)))
;;      (job (warbucks oliver) (administration big wheel)))
;; (and (supervisor (bitdiddle ben) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?type)))
;;      (job (warbucks oliver) (administration big wheel)))
