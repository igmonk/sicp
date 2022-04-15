;; Register
;;
;; A register is represented as a procedure with local state.
;; The procedure holds a value that can be accessed or changed.

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when trace-on
                 (newline)
                 (display (list 'register-name '= name
                                'old-contents '= contents
                                'new-contents '= value)))
               (set! contents value)))
            ((eq? message 'trace-on) (set! trace-on true))
            ((eq? message 'trace-off) (set! trace-on false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))


;; The following procedures are used to access registers:

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))


;; Register Syntax

(load "../5.2/list-utils.scm")

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
