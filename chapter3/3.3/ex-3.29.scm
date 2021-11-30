;; Exercise 3.29
;;
;; Another way to construct an or-gate is as a compound digital logic device,
;; built from and-gates and inverters.
;;
;; Define a procedure or-gate that accomplishes this.
;;
;; What is the delay time of the or-gate in terms of
;; and-gate-delay and inverter-delay?

(load "workbook-sdc.scm")

;; Boolean algebra:
;;
;; 1. The complement of the union of two sets is the same as
;;    the intersection of their complements:
;;
;;    not(A or B) = (not A) and (not B)
;;
;; 2. The complement of the intersection of two sets is the same as
;;    the union of their complements:
;;
;;    not(A and B) = (not A) or (not B)
;;
;; See Morgan laws:
;; https://en.wikipedia.org/wiki/De_Morgan%27s_laws
;;
;; Hence, given the 1st equation:
;;
;; A or B = not((not A) and (not B))

(define (or-gate a1 a2 output)
  (let ((na1 (make-wire))
        (na2 (make-wire))
        (and-out (make-wire)))
    (inverter a1 na1)
    (inverter a2 na2)
    (and-gate na1 na2 and-out)
    (inverter and-out output)
    'ok))


;; Run the simulation
;;
;; (define the-agenda (make-agenda))

;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))

;; (probe 'sum sum)     ; sum 0 New-value = 0
;; (probe 'carry carry) ; carry 0 New-value = 0

;; (half-adder input-1 input-2 sum carry) ; ok

;; (set-signal! input-1 1) ; done
;; (propagate)
;; sum 5 New-value = 1
;; sum 10 New-value = 0
;; sum 10 New-value = 1
;; done

;; (set-signal! input-2 1)
;; (propagate)
;; carry 13 New-value = 1
;; sum 18 New-value = 0
;; done

;; The delay of the or-gate in terms of and-gate-delay and inverter-delay is:
;;
;; delay(or) = 1 delay(inv) [2 parallel inverters] +
;;             1 delay(and) +
;;             1 delay(inv)
;;
;; delay(or) = 2 + 3 + 2 = 7 (2 more than the original one).
;;
;; The simulation shows the sum calculation took 2 time units more.
