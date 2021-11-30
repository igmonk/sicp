;; Exercise 3.28
;;
;; Define an or-gate as a primitive function box.
;; Your or-gate constructor should be similar to and-gate.

(load "workbook-sdc.scm")

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
  (cond ((not (and digital-signal? s1) (digital-signal? s2))
         (error "Invalid signals" s1 s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

;; (logical-or 0 0) ; 0
;; (logical-or 0 1) ; 1
;; (logical-or 1 0) ; 1
;; (logical-or 1 1) ; 1
;; (logical-or 1 2) ; Error: Invalid signals 1 2
;; (logical-or 3 4) ; Error: Invalid signals 3 4
