;; Exercise 5.39
;;
;; Write a procedure lexical-address-lookup that implements
;; the new lookup operation.
;;
;; It should take two arguments - a lexical address and
;; a run-time environment - and return the value of the variable
;; stored at the specified lexical address.
;;
;; Lexical-address-lookup should signal an error if the value of
;; the variable is the symbol *unassigned*. [46]
;;
;; Also write a procedure lexical-address-set! that implements
;; the operation that changes the value of the variable
;; at a specified lexical address.
;;
;; [46] This is the modification to variable lookup required if
;;      we implement the scanning method to eliminate internal
;;      definitions (exercise 5.43). We will need to eliminate
;;      these definitions in order for lexical addressing to work.


;; Start with the data structure for lexical address:

(define (make-lexical-addr frame-num displacement)
  (cons frame-num displacement))

(define (lexical-addr-frame-num lexical-addr) (car lexical-addr))
(define (lexical-addr-displacement lexical-addr) (cdr lexical-addr))


;; Next, implement the address lookup and set! procedures:

(define (lexical-addr-lookup lexical-addr env)
  (let* ((binding (lexical-addr-binding-lookup lexical-addr env))
         (value (frame-binding-value binding)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable -- LEXICAL-ADDR-LOOKUP" lexical-addr)
        value)))

(define (lexical-addr-set! lexical-addr val env)
  (let ((binding (lexical-addr-binding-lookup lexical-addr env)))
    (if (not binding)
        (error "Unbound variable -- LEXICAL-ADDR-SET!" lexical-addr)
        (begin
          (frame-binding-set-value! val binding)
          'ok))))

(define (lexical-addr-binding-lookup lexical-addr env)
  (let ((frame-num (lexical-addr-frame-num lexical-addr)))
    (bindings-ref (lexical-addr-displacement lexical-addr)
                  ((env-ref env frame-num) 'get-frame))))

(define (env-ref env index)
  (cond ((<= index 0) env)
        ((env 'empty?) (error "Empty environment -- ENV-REF"))
        (else (env-ref (env 'get-base-env) (- index 1)))))


;; Extend the frame representation with the binding-ref procedure
;; that returns the n-th element of the binding list, using
;; zero-origin indexing (see: ../5.4/frame.scm):

;; inner
(define (bindings-ref index)
  (list-ref bindings index))

;; interface
(define (bindings-ref index frame)
  ((frame 'bindings-ref) index))


;; Tests

(load "../5.4/environment")
(load "lexical-address.scm")


;; Consider the following code structure:

((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) <e1>)
      <e2>
      (+ c d x))))
 3
 4)


;; Test: <e1>

(define env1
  (make-environment-with
   (make-frame
    (make-frame-bindings (list 'y 'z) (list 10 11)))
   (make-environment-with
    (make-frame
     (make-frame-bindings (list 'a 'b 'c 'd 'e) (list 5 6 7 8 9)))
    (make-environment-with
     (make-frame
      (make-frame-bindings (list 'x 'y) (list 3 4)))
     (make-environment)))))


(lexical-addr-lookup (make-lexical-addr 0 0) env1) ; 10
(lexical-addr-lookup (make-lexical-addr 0 1) env1) ; 11

(lexical-addr-lookup (make-lexical-addr 1 0) env1) ; 5
(lexical-addr-lookup (make-lexical-addr 1 1) env1) ; 6
(lexical-addr-lookup (make-lexical-addr 1 2) env1) ; 7
(lexical-addr-lookup (make-lexical-addr 1 3) env1) ; 8
(lexical-addr-lookup (make-lexical-addr 1 4) env1) ; 9

(lexical-addr-lookup (make-lexical-addr 2 0) env1) ; 3
(lexical-addr-lookup (make-lexical-addr 2 1) env1) ; 4


;; Test: <e2>

(define env2 (env1 'get-base-env))

(lexical-addr-lookup (make-lexical-addr 0 0) env2) ; 5
(lexical-addr-lookup (make-lexical-addr 0 1) env2) ; 6
(lexical-addr-lookup (make-lexical-addr 0 2) env2) ; 7
(lexical-addr-lookup (make-lexical-addr 0 3) env2) ; 8
(lexical-addr-lookup (make-lexical-addr 0 4) env2) ; 9

(lexical-addr-lookup (make-lexical-addr 1 0) env2) ; 3
(lexical-addr-lookup (make-lexical-addr 1 1) env2) ; 4


;; Test: corner cases

(lexical-addr-lookup (make-lexical-addr 0 10) env1) ; The object (), passed...
(lexical-addr-lookup (make-lexical-addr 10 0) env1) ; Empty environment -- ENV-REF

(define env3
  (make-environment-with
   (make-frame
    (make-frame-bindings (list 'a) (list '*unassigned*)))
   (make-environment)))

(lexical-addr-lookup
 (make-lexical-addr 0 0)
 env3) ; Unassigned variable -- LEXICAL-ADDR-LOOKUP (0 . 0)


;; Test: lexical address set!

(lexical-addr-set! (make-lexical-addr 0 0) 100 env3) ; ok
(lexical-addr-lookup (make-lexical-addr 0 0) env3)   ; 100
(lexical-addr-set! (make-lexical-addr 0 0) 200 env3) ; ok
(lexical-addr-lookup (make-lexical-addr 0 0) env3)   ; 200
