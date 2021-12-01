;; Exercise 3.31
;;
;; The internal procedure accept-action-procedure! defined in make-wire
;; specifies that when a new action procedure is added to a wire,
;; the procedure is immediately run.
;;
;; Explain why this initialization is necessary.
;; In particular, trace through the half-adder example
;; in the paragraphs above and say how the system's response
;; would differ if we had defined accept-action-procedure! as
;;
;; (define (accept-action-procedure! proc)
;;   (set! action-procedures (cons proc action-procedures)))


;; There are only 2 places where action procedures are scheduled
;; for execution:
;; 1) when a digital gate is created
;; 2) when the digital signal on a given wire changes
;;
;; Below is the first one considered.
;;
;; Each newly added procedure is immediately run to ensure
;; proper initialisation: the procedure is added to the agenda.
;; Otherwise, the initial signal values would not be propagated
;; during the simulation.

(load "workbook-sdc.scm")

;; Test 1: with init
;;
;; (define the-agenda (make-agenda))
;;
;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))
;;
;; (probe 'sum sum)     ; sum 0 New-value = 0
;; (probe 'carry carry) ; carry 0 New-value = 0
;;
;; (half-adder input-1 input-2 sum carry) ; ok
;;
;; (set-signal! input-1 1) ; done
;; (propagate)
;;
;; sum 8 New-value = 1
;; done
;;
;; (set-signal! input-2 1)
;; (propagate)
;;
;; carry 11 New-value = 1
;; sum 16 New-value = 0
;; done


;; Test 2: without init
;;
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-proc! proc)
      (set! action-procedures (cons proc action-procedures)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc!)
            (else
             (error "Unknown operation -- WIRE" m))))
    dispatch))

;; (define the-agenda (make-agenda))
;;
;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))
;;
;; (probe 'sum sum)     ; ()
;; (probe 'carry carry) ; ()
;;
;; (half-adder input-1 input-2 sum carry) ; ok
;;
;; (set-signal! input-1 1) ; done
;; (propagate) ; done
;;
;; (set-signal! input-2 1)
;; (propagate)
;;
;; carry 11 New-value = 1
;; done
