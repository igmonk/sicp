;; Exercise 3.35
;;
;; Ben Bitdiddle tells Louis that one way to avoid the trouble
;; in exercise 3.34 is to define a squarer as a new primitive constraint.
;;
;; Fill in the missing portions in Ben's outline for a procedure
;; to implement such a constraint:
;;
;; (define (squarer a b)
;;   (define (process-new-value)
;;     (if (has-value? b)
;;         (if (< (get-value b) 0)
;;             (error "square less than 0 -- SQUARER" (get-value b))
;;             <alternative1>)
;;         <alternative2>))
;;   (define (process-forget-value) <body1>)
;;   (define (me request) <body2>)
;;   <rest of definition>
;;   me)


(load "workbook.scm")

(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? a)
           (set-value! b (square (get-value a)) me))
          ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0 -- SQUARER" (get-value b))
               (set-value! a (sqrt (get-value b)) me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


;; Tests
;;
;; (define a (make-connector))
;; (define b (make-connector))

;; (probe "a" a)
;; (probe "b" b)

;; (squarer a b)

;; (set-value! a 5 'user)
;;
;; Probe: b = 25
;; Probe: a = 5

;; (forget-value! a 'user)
;;
;; Probe: b = ?
;; Probe: a = ?

;; (set-value! a 6 'user)
;;
;; Probe: b = 36
;; Probe: a = 6

;; (forget-value! a 'user)
;;
;; Probe: b = ?
;; Probe: a = ?

;; (set-value! b 49 'user)
;;
;; Probe: a = 7
;; Probe: b = 49

;; (forget-value! b 'user)
;;
;; Probe: a = ?
;; Probe: b = ?

;; (set-value! b 64 'user)
;;
;; Probe: a = 8
;; Probe: b = 64

;; (forget-value! b 'user)
;;
;; Probe: a = ?
;; Probe: b = ?

;; (set-value! b -64 'user) ;; Error: square less than 0 -- SQUARER -64


;; It's worth noticing, the built-in sqrt procedure produces complex numbers
;; in case it is given a negative number as input:
;;
;; (sqrt -64) ; +8i
;;
;; Hence, even without the check for negative values of the square,
;; the squarer constraint continues to serve the purpose:
;;
;; (set-value! b -64 'user)
;;
;; Probe: a = +8i
;; Probe: b = -64
