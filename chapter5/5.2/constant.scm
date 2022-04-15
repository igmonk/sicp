;; Constant

(load "../5.2/list-utils.scm")

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
