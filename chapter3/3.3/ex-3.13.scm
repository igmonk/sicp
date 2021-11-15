;; Exercise 3.13
;;
;; Consider the following make-cycle procedure,
;; which uses the last-pair procedure defined in exercise 3.12:

(load "ex-3.12.scm")

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; Draw a box-and-pointer diagram that shows the structure z
;; created by

(define z (make-cycle (list 'a 'b 'c)))

;; What happens if we try to compute (last-pair z)?


;; Box-and-pointer diagram that shows the structure z
;;
;;        ___________________
;;       |                   |
;;       ↓                   |
;; z -> |x|x|--->|x|x|--->|x|x|
;;       |        |        |
;;       a        b        c


;; If we try to compute (last-pair z), the process ends up in
;; the infinite loop, since the structure z does not have
;; a proper ending: the cdr of its last pair references
;; the first pair of the list.
