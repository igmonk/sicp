;; Exercise 3.32
;;
;; The procedures to be run during each time segment of the agenda
;; are kept in a queue. Thus, the procedures for each segment
;; are called in the order in which they were added to the agenda
;; (first in, first out).
;;
;; Explain why this order must be used.
;; 
;; In particular, trace the behavior of an and-gate whose inputs
;; change from 0,1 to 1,0 in the same segment and say how the behavior
;; would differ if we stored a segment's procedures in an ordinary list,
;; adding and removing procedures only at the front (last in, first out).


;; The task invites to explore the domain of logic hazards:
;;
;;   Logic hazards are manifestations of a problem in which changes in
;;   the input variables do not change the output correctly due to
;;   some form of delay caused by logic elements.
;;
;; More specifically, static hazards:
;;
;;   A static hazard is the situation where, when one input variable changes,
;;   the output changes momentarily before stabilizing to the correct value.
;;
;; There are two types of static hazards:
;;
;; 1. Static-1 Hazard: the output is currently 1 and after the inputs change,
;;                     the output momentarily changes to 0,1
;;                     before settling on 1
;;
;; 2. Static-0 Hazard: the output is currently 0 and after the inputs change,
;;                     the output momentarily changes to 1,0
;;                     before settling on 0
;;
;; See: https://en.wikipedia.org/wiki/Hazard_(logic)
;;
;; The task suggests testing Static-0 Hazard.


;; Below are some tests tracing the behaviour of an and-gate.

(load "workbook-sdc.scm")

;; 1. A queue-based simulation (default)
;;
;; (define the-agenda (make-agenda))

;; (define in1 (make-wire))
;; (define in2 (make-wire))
;; (define out1 (make-wire))

;; (probe 'out1 out1) ; out1 0 New-value = 0

;; (and-gate in1 in2 out1)

;; Set 1: in1 = 0, in2 = 1
;; (set-signal! in1 0)
;; (set-signal! in2 1) 

;; (propagate)

;; Set 2: in1 = 1, in2 = 0
;; (set-signal! in1 1)
;; (set-signal! in2 0)

;; (propagate)

;; out1 6 New-value = 1
;; out1 6 New-value = 0


;; 2. An ordinary list based simulation
;;
;; Instead of re-implementing the agenda internals to make it work with
;; ordinary lists and adding/removing procedures only at the front (LIFO),
;; the same effect can be achieved by switching the order of signal
;; assignments before the second set action:
;;
;; Set 1: in1 = 0, in2 = 1
;; Set 2: in2 = 0, in1 = 0

;; (define the-agenda (make-agenda))

;; (define in1 (make-wire))
;; (define in2 (make-wire))
;; (define out1 (make-wire))

;; (probe 'out1 out1) ; out1 0 New-value = 0

;; (and-gate in1 in2 out1)

;; Set 1: in1 = 0, in2 = 1
;; (set-signal! in1 0)
;; (set-signal! in2 1) 

;; (propagate)

;; Set 2: in2 = 1, in1 = 0  <--  The opposite order of signal assignments.
;; (set-signal! in2 0)
;; (set-signal! in1 1)

;; (propagate) ; done
