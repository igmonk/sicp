;; Label

(load "list-utils.scm")

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))
