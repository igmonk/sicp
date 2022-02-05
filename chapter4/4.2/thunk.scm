;; Thunk

(load "list-utils.scm")

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (evaluate-thunk! thunk eval-fn)
  (let ((result (eval-fn (thunk-exp thunk)
                         (thunk-env thunk))))
    (set-car! thunk 'evaluated-thunk)
    (set-car! (cdr thunk) result) ; replace exp with its value
    (set-cdr! (cdr thunk) '())    ; forget unneeded env
    result))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
