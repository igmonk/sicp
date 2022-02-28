;; Variables & Constant symbols

(load "list-utils.scm")

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))
