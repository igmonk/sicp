;; Lexical address
;;
;; The data structure represents the lexical address of a variable
;; and consists of two numbers:
;; - a frame number, which specifies how many frames to pass over
;; - a displacement number, which specifies how many variables to
;;   pass over in that frame

(define (make-lexical-addr frame-num displacement)
  (cons frame-num displacement))

(define (lexical-addr-frame-num lexical-addr) (car lexical-addr))
(define (lexical-addr-displacement lexical-addr) (cdr lexical-addr))
