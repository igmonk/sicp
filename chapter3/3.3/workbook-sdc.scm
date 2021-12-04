;; 3.3.4 A Simulator for Digital Circuits
;;
;; Digital systems are constructed by interconnecting simple elements.
;; Although the behavior of these individual elements is simple,
;; networks of them can have very complex behavior.
;;
;; Computer simulation of proposed circuit designs is an important tool
;; used by digital systems engineers.
;;
;; In this section we design a system for performing digital logic simulations.
;; This system typifies a kind of program called an 'event-driven simulation',
;; in which actions ("events") trigger further events that happen at a later time,
;; which in turn trigger more events, and so so.

;; Our computational model of a circuit will be composed of objects that
;; correspond to the elementary components from which the circuit is constructed:
;; - wires           -> "hold" the signals
;; - digital signals -> at any moment have only one of two possible values, 0 and 1
;; - function boxes  -> connect wires carrying input signals to other output wires
;;
;; We can connect primitive functions together to construct more complex functions.
;; To accomplish this we wire the outputs of some function boxes to the inputs
;; of other function boxes.

;; For example, the half-adder circuit shown below consists of
;; - an or-gate
;; - two and-gates
;; - an inverter
;;
;; It takes two input signals, A and B, and has two output signals, S and C.
;; S will become 1 whenever precisely one of A and B is 1, and
;; C will become 1 whenever A and B are both 1.

;;               ┌  ┐ D                ┌   ┐
;; A ------┬-----|OR|------------------|AND|----- S
;;       ┌-|-----└  ┘              ┌---└   ┘
;;       | |               ┌-  ┐ E |
;;       | |           ┌---|NOT|---┘
;;       | |           |   └   ┘
;;       | └--┌   ┐    |
;; B ----┴----|AND|----┴------------------------- C
;;            └   ┘
;;
;; Pic: A half-added circuit

;; Define a procedure half-adder that constructs this circuit:

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; The output signal is delayed by a time that depends on the type of the function box.
;; because of the delays involved, the outputs may be generated at different times.
;; Many of the difficulties in the design of digital circuits arise from this fact.

;; Now we can use half-adder as a building block in creating more complex circuits.
;; Below is shown a full-adder composed of two half-adders and an or-gate:
;;
;;                     ┌  ┐
;;    A ---------------|  |------------------ SUM
;;                     |HA|      ┌  ┐
;;                  ┌--|  |------|  |
;;            ┌  ┐  |  └  ┘      |OR|-------- C-out
;;    B ------|  |--┘            |  |
;;            |HA|---------------└  ┘
;; C-in ------|  |
;;            └  ┘
;;
;; Pic: A full-adder circuit
;; HA = Half-Adder

;; Define a procedure full-adder that constructs this circuit:

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c2 c1 c-out)
    'ok))

;; If we adopt the general perspective on languages with which we approached
;; the study of Lisp in section 1.1, we can say that
;; 1) the primitive function boxes form the primitive elements of the language,
;; 2) wiring boxes together provides a means of combination,
;; 3) specifying wiring patterns as procedures serves as a means of abstraction.

;; Primitive function boxes

;; The primitive function boxes implement the "forces" by which
;; a change in the signal on one wire influences the signals on other wires.
;;
;; To build function boxes, we use the following operations on wires:
;;
;; - (get-signal <wire>)
;;     returns the current value of the signal on the wire
;; - (set-signal! <wire> <new value>)
;;     changes the value of the signal on the wire to the new value
;; - (add-action! <wire> <procedure of no arguments>)
;;     asserts that the designated procedure should be run whenever the signal
;;     on the wire changes value. Such procedures are the vehicles by which
;;     changes in the signal value on the wire are communicated to other wires.
;;
;; In addition, we will make use of a procedure after-delay that
;; takes a time delay and a procedure to be run and executes the given procedure
;; after the given delay.
;;
;; Using these procedures we can define the primitive digital logic functions.

;; Inverter

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "Invalid signal" s))))

;; And-Gate

(define (digital-signal? s)
  (or (= s 0) (= s 1)))

(define (and-gate a1 a2 output)
  (define (and-action-proc)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc)
  'ok)

(define (logical-and s1 s2)
  (cond ((not (and (digital-signal? s1) (digital-signal? s2)))
         (error "Invalid signals" s1 s2))
        ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

;; (logical-and 0 0) ; 0
;; (logical-and 0 1) ; 0
;; (logical-and 1 0) ; 0
;; (logical-and 1 1) ; 1
;; (logical-and 1 2) ; Error: Invalid signals 1 2
;; (logical-and 3 4) ; Error: Invalid signals 3 4

;; Or-Gate

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

(define (logical-or s1 s2)
  (cond ((not (and (digital-signal? s1) (digital-signal? s2)))
         (error "Invalid signals" s1 s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

;; (logical-or 0 0) ; 0
;; (logical-or 0 1) ; 1
;; (logical-or 1 0) ; 1
;; (logical-or 1 1) ; 1
;; (logical-or 1 2) ; Error: Invalid signals 1 2
;; (logical-or 3 4) ; Error: Invalid signals 3 4

;; Representing wires
;;
;; A wire in our simulation will be a computational object with
;; two local state variables:
;; 1) a signal-value (initially taken to be 0) and
;; 2) a collection of action-procedures to be run
;;    when the signal changes value
;;
;; Below is its message-passing style representation:

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-proc! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc!)
            (else
             (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

;; With the local dispatch procedure set up as specified,
;; we can provide the following procedures to access
;; the local operations on wires:

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

;; These procedures are simply syntactic sugar that allow us to use
;; ordinary procedural syntax to access the local procedures of objects.
;;
;; We can interchange the role of "procedures" and "data" in such a simple way.
;;
;; For example, if we write (wire 'get-signal) we think of wire as a procedure
;; that is called with the message get-signal as input.
;; Alternatively, writing (get-signal wire) encourages us to think of wire
;; as a data object that is the input to a procedure get-signal.
;;
;; The truth of the matter is that, in a language in which we can deal
;; with procedures as objects, there is no fundamental difference between
;; "procedures" and "data", and we can choose our syntactic sugar
;; to allow us to program in whatever style we choose.

;; The wires are shared among the various devices that have been connected to them.
;; Thus, a change made by an interaction with one device will affect
;; all the other devices attached to the wire.
;; The wire communicates the change to its neighbours by calling the action procedures
;; provided to it when the connections were established.

;; The agenda
;;
;; The only thing needed to complete the simulator is 'after-delay'.
;;
;; The idea here is that we maintain a data structure, called an 'agenda',
;; that contains a schedule of things to do.

;; The following operations are defined for agendas:
;;
;; - (make-agenda)
;;     returns a new empty agenda
;; - (empty-agenda? <agenda>)
;;     is true if the specified agenda is empty
;; - (first-agenda-item <agenda>)
;;     returns the first item on the agenda
;; - (remove-first-agenda-item! <agenda>)
;;     modifies the agenda by removing the first item
;; - (add-to-agenda! <time> <action> <agenda>)
;;     modifies the agenda by adding the given action procedure
;;     to be run at the specified time
;; - (current-time <agenda>)
;;     returns the current simulation time

;; The procedure 'after-delay' adds new elementsto the particular agenda:

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; Implementing the agenda
;;
;; A data structure which holds the procedures that are scheduled for
;; future execution.
;;
;; The agenda is made up of time segments.
;; Each time segment is a pair consisting of a number (the time)
;; and a queue (ex. 3.32) that holds the procedures that are scheduled
;; to be run during that time segment.

(load "queue.scm")

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;; The agenda itself is a one-dimensional table of tine segments.
;; The segments are sorted in order of increasing time.
;;
;; The 'current time' is stored at the head of the agenda.
;; A newly constructed agenda has no time segments and
;; has a current time of 0:

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

;; To add an action to an agenda, we first check if the agenda is empty
;; If so, we create a time segment for the action and install this in the agenda.
;; Otherwise, we scan the agenda, examining the time of each segment.
;;
;; If we find a segment for our appointed time, we add the action
;; to the associated queue.
;; If we reach a time later than the one to which we are appointed,
;; we insert a new time segment into the agenda just before it.
;; If we reach the end of the agenda, we must create a new time segment
;; at the end.

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

;; The procedure that removes the first item from the agenda
;; deletes the item at the front of the queue in the first time segment.
;;
;; If this deletion makes the time segment empty,
;; we remove it from the list of segments:

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

;; The first agenda item is found at the head of the queue
;; in the first time segment.
;;
;; Whenever we extract an item, we also update the current time:

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; The simulation is driven by the procedure 'propagate', which
;; operates on 'the-agenda', executing each procedure on the agenda
;; in sequence.
;;
;; In general, as the simulation runs, new items will be added
;; to the agenda, and 'propagate' will continue the simulation
;; as long as there are items on the agenda:

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; A sample simulation
;;
;; The following procedure, which places a "probe" on a wire,
;; shows the simulator in action:

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;; We begin by initializing the agenda and specifying delays
;; for the primitive function boxes:

;; (define the-agenda (make-agenda))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Define four wires, placing probes on two of them:

;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))

;; (probe 'sum sum)     ; sum 0 New-value = 0
;; (probe 'carry carry) ; carry 0 New-value = 0

;; Connect the wires in a half-adder circuit,
;; set the signal on input-1 to 1, and run the simulation:

;; (half-adder input-1 input-2 sum carry) ; ok

;; (set-signal! input-1 1) ; done
;; (propagate)
;; sum 8 New-value = 1
;; done

;; (set-signal! input-2 1)
;; (propagate)
;; carry 11 New-value = 1
;; sum 16 New-value = 0
;; done
