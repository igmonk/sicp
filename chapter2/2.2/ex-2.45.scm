;; Exercise 2.45
;;
;; 'right-split' and 'up-split' can be expressed as instances of a general splitting operation.
;; Define a procedure 'split' with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

;; produces procedures 'right-split' and 'up-split' with the same behaviours as
;; the ones already defined.

(define (split combine-main combine-smaller)
  (lambda (painter n)
    (define (inner p i)
      (if (= i 0)
          p
          (let ((smaller (inner p (- i 1))))
            (combine-main p (combine-smaller smaller)))))
    (inner painter n)))
