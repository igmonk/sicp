;; Exercise 4.59
;;
;; Ben Bitdiddle has missed one meeting too many.
;; Fearing that his habit of forgetting meetings could cost him
;; his job, Ben decides to do something about it.

(load "test-utils.scm")
(load "microshaft.scm")

;; Ingest the assertions from 'microshaft.scm' into the data base.
(ingest-ms-assertions db)

;; Run the driver loop
(query-driver-loop)


;; He adds all the weekly meetings of the firm to the Microshaft
;; data base by asserting the following:

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))

;; Each of the above assertions is for a meeting of an entire division.
;;
;; Ben also adds an entry for the company-wide meeting that spans
;; all the divisions. All of the company's employees attend this meeting.

(assert! (meeting whole-company (Wednesday 4pm)))


;; a. On Friday morning, Ben wants to query the data base for all
;;    the meetings that occur that day. What query should he use?

(meeting ?x (Friday . ?time))

;; Query results:
;;
;; (meeting administration (friday |1pm|))


;; b. Alyssa P. Hacker is unimpressed. She thinks it would be much more
;;    useful to be able to ask for her meetings by specifying her name.
;;    So she designs a rule that says that a person's meetings include
;;    all whole-company meetings plus all meetings of that person's division.
;;    Fill in the body of Alyssa's rule.

(assert!
 (rule (meeting-time ?person ?day-and-time)
       (or (meeting whole-company ?day-and-time)
           (and (job ?person (?department . ?position))
                (meeting ?department ?day-and-time)))))

;; List all Alyssa's meetings
(meeting-time (Hacker Alyssa P) ?day-and-time)

;; Query results:
;;
;; (meeting-time (hacker alyssa p) (wednesday |4pm|))
;; (meeting-time (hacker alyssa p) (wednesday |3pm|))


;; List all Warbucks's meetings
(meeting-time (Warbucks Oliver) ?day-and-time)

;; Query results:
;;
;; (meeting-time (warbucks oliver) (wednesday |4pm|))
;; (meeting-time (warbucks oliver) (friday |1pm|))
;; (meeting-time (warbucks oliver) (monday |10am|))


;; c. Alyssa arrives at work on Wednesday morning and wonders what meetings
;;    she has to attend that day. Having defined the above rule,
;;    what query should she make to find this out?


(and (meeting-time (Hacker Alyssa P) (Wednesday . ?time))
     (meeting ?department (Wednesday . ?time)))

;; Query results:
;;
;; (and (meeting-time (hacker alyssa p) (wednesday |4pm|))
;;      (meeting whole-company (wednesday |4pm|)))
;; (and (meeting-time (hacker alyssa p) (wednesday |3pm|))
;;      (meeting computer (wednesday |3pm|)))


;; Alternatively, define a rule that lists all the meetings
;; the given person has on the given day.
(assert!
 (rule (todays-meeting ?person ?department ?today ?time)
       (and (meeting-time ?person (?today . ?time))
            (meeting ?department (?today . ?time)))))


;; List all Alyssa's meetings on Wednesday
(todays-meeting (Hacker Alyssa P) ?department Wednesday ?time)

;; Query results:
;;
;; (todays-meeting (hacker alyssa p) whole-company wednesday (|4pm|))
;; (todays-meeting (hacker alyssa p) computer wednesday (|3pm|))


;; List all Warbucks's meetings on Wednesday
(todays-meeting (Warbucks Oliver) ?department Wednesday ?time)

;; Query results:
;;
;; (todays-meeting (warbucks oliver) whole-company wednesday (|4pm|))
