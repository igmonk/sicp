;; 3.3.5 Propagation of Constraints
;;
;; This section sketches the design of a language that enables us to work
;; in terms of relations themselves.
;;
;; The primitive elements of the language are:
;; - primitive constraints
;; - constraint networks
;; - connectors
;;
;; Primitive constraints state that certain relations hold between quantities.
;;
;; Constraints are combined by constructing constraint networks, in which
;; constraints are joined by connectors.
;;
;; A connector is an object that 'holds' a value that may participate in
;; one or more constraints.

;; For example, we know that the relationship between Fahrenheit and Celsius
;; temperatures is
;;
;; 9C = 5(F - 32)
;;
;; Such a constraint can be thought of as a network consisting of primitive adder,
;; multiplier, and constant constraints:
;;
;;
;;        ┌--------┐       ┌--------┐   v   ┌--------┐
;; C -----| m1     |   u   |     m1 |-------| a1     |
;;        |    * p |-------| p *    |       |    + s |------ F
;;     ┌--| m2     |       |     m2 |--┐ ┌--| a2     |
;;     |  └--------┘       └--------┘  | |  └--------┘
;;    w|                             x | | y
;;     |    ┌---┐             ┌---┐    | |    ┌----┐
;;     └----| 9 |             | 5 |----┘ └----| 32 |
;;          └---┘             └---┘           └----┘
;;
;;
;; Computation by such a network proceeds as follows:
;;
;;   When a connector is given a value (by the user or by a constraint box
;;   to which it is linked), it awakens all of its associated constraints
;;   (except for the constraint that just awakened it) to inform them
;;   that it has a value.
;;
;;   Each awakened constraint box then polls its connectors to see
;;   if there is enough information to determine a value for a connector.
;;   If so, the box sets that connector, which then awakens all of
;;   its associated constraints, and so on.

;; For instance, in conversion between Celsius and Fahrenheit, w, x and y
;; are immediately set by the constant boxes to 9, 5, and 32, respectively.
;;
;; The connectors awaken the multipliers and the adder, which determine that
;; there is not enough information to proceed.
;;
;; If the user (or some other part of the network) sets C to a value (say 25),
;; the leftmost multiplier will be awakened, and it will set u to 25 * 9 = 225.
;; Then u awakens the second multiplier, which sets v to 45,
;; and v awakens the adder, which sets F to 77.


;; Using the constraint system
;;
;; To use the constraint system to carry out the temperature computation
;; outlined above, we first create two connectors, C and F, by calling
;; the constructor make-connector, and link C and F in an appropriate network:

;; (define C (make-connector))
;; (define F (make-connector))

;; (celsius-fahrenheit-converter C F)

;; The procedure that creates the network is defined as follows:

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;; This procedure creates the internal connectors u, v, w, x, and y, and
;; links them as using the primitive constraint constructors adder, multiplier,
;; and constant.

;; To watch the network in action, we can place probes on the connectors
;; C and F, using a 'probe' procedure.
;; Placing a probe on a connector will cause a message to be printed
;; whenever the connector is given a value:

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

;; Next we set the value of C to 25.
;; (the directive comes from the 'user')
;;
;; (set-value! C 25 'user)

;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77

;; The probe on C awakens and reports the value.
;; C also propagates its value through the network as described above.

;; Not we can try to set F to a new value, say 212:
;;
;; (set-value! F 212 'user)

;; Error: Contradiction (77 212)

;; The connector complains that it has sensed a contradiction:
;; Its value is 77, and someone is trying to set it to 212.
;;
;; If we really want to reuse the network with new values,
;; we can tell C to forget its old value:
;;
;; (forget-value! C 'user)

;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?

;; C finds that the user, who set its value originally,
;; is now retracting that value, so C agrees to lose its value,
;; as shown by the probe, and informs the rest of the network of this fact.
;;
;; This information eventually propagates to F,
;; which now finds that it has no reason for continuing to believe
;; that its own value is 77.
;; Thus, F also gives up its value, as shown by the probe.

;; Now that F has no value, we are free to set it to 212:
;;
;; (set-value! F 212 'user)

;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100

;; This new value, when propagated through the network,
;; forces C to have a value of 100, and this is registered by the probe on C.
;;
;; Notice that the very same network is being used to compute C given F
;; and to compute F given C.
;;
;; This nondirectionality of computation is the distinguishing feature
;; of constraint-based systems.


;; Implementing the constraint system
;;
;; The constraint system is implemented via procedural objects with local state,
;; in a manner very similar to the digital-circuit simulator of section 3.3.4.

;; The basic operations on connectors are the following:
;;
;; - (has-value? <connector>)
;;     tells whether the connector has a value
;; - (get-value <connector>)
;;     returns the connector's current value
;; - (set-value! <connector> <new-value> <informant>)
;;     indicates that the informant is requesting the connector
;;     to set its value to the new value
;; - (forget-value! <connector> <retractor>)
;;     tells the connector that the retractor is requesting it
;;     to forget its value
;; - (connect <connector> <new-constraint>)
;;     tells the connector to participate in the new constraint

;; The connectors communicate with the constraints by means of the procedures
;;
;; - inform-about-value
;;     which tells the given constraint that the connector has a value
;; - inform-about-no-value
;;     which tells the constraint that the connector has lost its value


;; Constraint: adder

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


;; Syntax interfaces

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


;; Constraint: multiplier

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value)) ;; <- ???
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)


;; Constraint: constant
;; Sets the value of the designated connector.
;;
;; The setter/informant for this type of constraint can only be
;; the constant constraint itself. This forces a connector avoid
;; invoking 'inform-about-no-value' upon a request from any other
;; constraint.

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me) ;; <-- constant acts as the only setter
  me)


;; Constraint: probe
;; Prints a message about the setting or unsetting of the designated connector.

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)


;; Representing connectors
;;
;; A connector is represented as a procedural object with local state variables:
;; - value -> the current value of the connector
;; - informant -> the object that set the connector's value
;; - constraints -> a list of the constraints in which the connector participates

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else
             (error "Unknown operation -- CONNECTOR" request))))
    me))


(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))


;; A syntax interface for the connectors' dispatch:

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
