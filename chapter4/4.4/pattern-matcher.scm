;; Pattern Matcher

(load "frame.scm")

;; The pattern matcher used by the query system takes as inputs
;; a pattern, a datum, and a frame that specifies bindings for
;; various pattern variables.
;;
;; It checks whether the datum matches the pattern in a way
;; that is consistent with the bindings already in the frame.

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

;; The stored value may contain pattern variables if
;; it was stored during unification (see section 4.4.4.4).
;;
;; The recursive match of the stored pattern against the new data
;; will add or check bindings for the variables in this pattern.

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))
