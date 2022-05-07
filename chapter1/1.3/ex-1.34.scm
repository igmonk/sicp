;; Exercise 1.34
;;
;; Suppose we define the procedure

(define (f g)
  (g 2))

(load "../../common.scm")

;; Then we have
;;
;; (f square)                     ; 4
;; (f (lambda (z) (* z (+ z 1)))) ; 6
;;
;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

;; During evaluation of (f f), the interpreter makes another invocation of f in the form (f 2),
;; since the variable g is bound to f.
;; In turn, (f 2) makes the interpreter bind g to 2 and try to evaluate (2 2), which fails
;; due to the inapplicability of a number.
;;
;; The exact message returned by the interpreter:
;; The object 2 is not applicable.
