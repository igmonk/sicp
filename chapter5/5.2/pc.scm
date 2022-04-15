;; Program Counter

(load "../5.2/register.scm")

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))
