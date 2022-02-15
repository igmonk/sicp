;; Evaluator Test

(load "test-utils.scm")

;; Create the global environment
(define env (setup-environment))
(define (reset-env!) (set! env (setup-environment)) 'ok)

;; Create an evaluator
(define evaluator (create-new-evaluator))
(define (ambeval-exp exp) ((evaluator 'ambeval)
                           exp
                           env
                           (lambda (value fail) value)
                           (lambda () 'failed)))

;; Run REPL
(driver-loop env (evaluator 'ambeval))


;; The requirement that a particular predicate expression p must
;; be true (required in the majority of test runs and exercises):
(define (require p)
  (if (not p) (amb)))


;; Test: self-evaluating
(ambeval-exp '100001) ; 100001
(ambeval-exp "hello") ; "hello"


;; Test: variable
(ambeval-exp 'car)   ; (primitive #[compiled-procedure ...
(ambeval-exp 'true)  ; true
(ambeval-exp 'false) ; false


;; Test: quoted
(ambeval-exp '(quote test1)) ; test1
(ambeval-exp '(quote test2)) ; test2


;; Test: definition
(ambeval-exp '(define x 1)) ; ok
(ambeval-exp 'x)            ; 1
(ambeval-exp '(define x 2)) ; ok
(ambeval-exp 'x)            ; 2
(reset-env!)


;; Test: assignment
(ambeval-exp 'x)            ; Unbound variable x
(ambeval-exp '(define x 1)) ; ok
(ambeval-exp 'x)            ; 1
(ambeval-exp '(set! x 2))   ; ok
(ambeval-exp 'x)            ; 2
(reset-env!)


;; Test: undefine
(ambeval-exp '(let ((x 100))
                (let ((y 200))
                  (define x y)
                  (undefine x)
                  x))) ; 100


;; Test: if
(ambeval-exp '(if true 1 2))  ; 1
(ambeval-exp '(if true 2 1))  ; 2
(ambeval-exp '(if false 3 4)) ; 4
(ambeval-exp '(if false 4 3)) ; 3
(ambeval-exp '(if true 5))    ; 5
(ambeval-exp '(if false 5))   ; false


;; Test: unless
(ambeval-exp '(unless true 1 2))  ; 2
(ambeval-exp '(unless true 2 1))  ; 1
(ambeval-exp '(unless false 3 4)) ; 3
(ambeval-exp '(unless false 4 3)) ; 4
(ambeval-exp '(unless true 5))    ; false
(ambeval-exp '(unless false 5))   ; 5


;; Test: begin
(ambeval-exp '(begin 1 2 3)) ; 3
(ambeval-exp '(begin 4 5 6)) ; 6

(ambeval-exp '(begin 1 2 (+ 3 4 5))) ; 12
(ambeval-exp '(begin 1 2 (* 3 4 5))) ; 60


;; Test: cond
(ambeval-exp '(cond ((= 1 1) 100)
                    ((= 1 2) 200)
                    (else 300))) ; 100

(ambeval-exp '(cond ((= 1 2) 100)
                    ((= 1 1) 200)
                    (else 300))) ; 200

(ambeval-exp '(cond ((= 1 2) 100)
                    ((= 1 3) 200)
                    (else 300))) ; 300

(ambeval-exp '(cond ((cons 1 2) => car)
                    (else 'wrong))) ; 1


;; Test: and
(ambeval-exp '(and))                 ; true
(ambeval-exp '(and true true true))  ; true
(ambeval-exp '(and true true false)) ; false
(ambeval-exp '(and true true 100))   ; 100
(ambeval-exp '(and 100 200 300))     ; 300
(ambeval-exp '(and false true true)) ; false


;; Test: and (derived)
(ambeval-exp '(and-d))                 ; true
(ambeval-exp '(and-d true true true))  ; true
(ambeval-exp '(and-d true true false)) ; false
(ambeval-exp '(and-d true true 100))   ; 100
(ambeval-exp '(and-d 100 200 300))     ; 300
(ambeval-exp '(and-d false true true)) ; false


;; Test: or
(ambeval-exp '(or))                 ; false
(ambeval-exp '(or true true true))  ; true
(ambeval-exp '(or true true false)) ; true
(ambeval-exp '(or true true 100))   ; true
(ambeval-exp '(or 100 200 300))     ; 100
(ambeval-exp '(or false true true)) ; true


;; Test: or (derived)
(ambeval-exp '(or-d))                 ; false
(ambeval-exp '(or-d true true true))  ; true
(ambeval-exp '(or-d true true false)) ; true
(ambeval-exp '(or-d true true 100))   ; true
(ambeval-exp '(or-d 100 200 300))     ; 100
(ambeval-exp '(or-d false true true)) ; true


;; Test: xor
(ambeval-exp '(xor false false)) ; false
(ambeval-exp '(xor false true))  ; true
(ambeval-exp '(xor true false))  ; true
(ambeval-exp '(xor true true))   ; false


;; Test: lambda
(ambeval-exp '(lambda (x y) (cons x y)))       ; (procedure (x y ((cons x y)) ...
(ambeval-exp '((lambda (x y) (cons x y)) 1 2)) ; (1 . 2)


;; Test: let
(ambeval-exp '(let ((x 1) (y 2)) (cons x y)))     ; (1 . 2)
(ambeval-exp '(let ((id (lambda (x) x))) (id 1))) ; 1
(ambeval-exp '(let ((id (lambda (x) x))) (id 2))) ; 2


;; Test: let (nested)
(ambeval-exp '(let ((x 3))
                (let ((y (+ x 2)))
                  (let ((z (+ x y 5)))
                    (* x z))))) ; 39


;; Test: let (named)
(ambeval-exp '(define (fib n)
                (let fib-iter ((a 1)
                               (b 0)
                               (count n))
                  (if (= count 0)
                      b
                      (fib-iter (+ a b) a (- count 1))))))

(ambeval-exp '(fib 1)) ; 1
(ambeval-exp '(fib 2)) ; 1
(ambeval-exp '(fib 3)) ; 2
(ambeval-exp '(fib 4)) ; 3
(ambeval-exp '(fib 5)) ; 5
(ambeval-exp '(fib 6)) ; 8
(ambeval-exp '(fib 7)) ; 13
(ambeval-exp '(fib 8)) ; 21
(ambeval-exp '(fib 9)) ; 34
(reset-env!)


;; Test: let*
(ambeval-exp '(let* ((x 3)
                     (y (+ x 2))
                     (z (+ x y 5)))
                (* x z))) ; 39

(ambeval-exp '(let* ((x 3)) (* x x))) ; 9


;; Test: letrec
;; TODO: fix Unassigned variable fact
(ambeval-exp '(letrec ((fact
                        (lambda (n)
                          (if (= n 1)
                              1
                              (* n (fact (- n 1)))))))
                (fact 10))) ; 3628800

;; TODO: fix Unassigned variable even?
(ambeval-exp '(letrec ((even?
                        (lambda (n)
                          (if (= n 0)
                              true
                              (odd? (- n 1)))))
                       (odd?
                        (lambda (n)
                          (if (= n 0)
                              false
                              (even? (- n 1))))))
                (even? 5))) ; false


;; Test: while
(ambeval-exp '(define (factorial x)
                (let ((n 1)
                      (i x))
                  (while (> i 0)
                         (set! n (* n i))
                         (set! i (- i 1)))
                  n)))

(ambeval-exp '(factorial 1)) ; 1
(ambeval-exp '(factorial 2)) ; 2
(ambeval-exp '(factorial 3)) ; 6
(ambeval-exp '(factorial 4)) ; 24
(ambeval-exp '(factorial 5)) ; 120
(reset-env!)


;; Test: application (primitive procedure)
(ambeval-exp '(not true))         ; false
(ambeval-exp '(not false))        ; true
(ambeval-exp '(= 100 100))        ; true
(ambeval-exp '(= 100 200))        ; false
(ambeval-exp '(not (= 100 100)))  ; false
(ambeval-exp '(not (= 100 200)))  ; true
(ambeval-exp '(eq? 'abc 'abc))    ; true
(ambeval-exp '(eq? 'abc 'def))    ; false
(ambeval-exp '(cons 1 2))         ; (1 . 2)
(ambeval-exp '(car (cons 1 2)))   ; 1
(ambeval-exp '(cdr (cons 1 2)))   ; 2
(ambeval-exp '(null? '()))        ; true
(ambeval-exp '(null? (cons 1 2))) ; false


;; Test: application (compound procedure)
(ambeval-exp '(define (append x y)
                (if (null? x)
                    y
                    (cons (car x)
                          (append (cdr x) y))))) ; ok

(ambeval-exp 'append)                     ; (procedure (x y) ((if (null? x) ...
(ambeval-exp '(append '(a b c) '(d e f))) ; (a b c d e f)
(reset-env!)


;; Test: unassigned
(ambeval-exp '(define x '*unassigned*))
(ambeval-exp 'x) ; Unassigned variable x
(reset-env!)


;; Below are some experiments to compare the speed of the original
;; metacircular evaluator (evaluation is mixed with syntactic analysis)
;; with the version in the section 4.1.7 (syntactic analysis precedes
;; evaluation).

;; Test method
(ambeval-exp '(define (factorial n)
                (if (= n 1)
                    1
                    (* (factorial (- n 1)) n))))

;; Time values:
;; - run-time  -> the elapsed run time
;; - gc-time   -> the amount of time spent in the garbage collector
;; - real-time -> the elapsed real time
;;
;; All three times are in ticks.
;; A tick is a unit of time that is unspecified here but
;; can be converted to and from seconds by supplied procedures.

;; 1. Preceding syntactic analysis
(measure-run-time (lambda () (ambeval-exp '(factorial 1000)))) ;; .12 0. .12
(measure-run-time (lambda () (ambeval-exp '(factorial 2000)))) ;; .22 0. .226
(measure-run-time (lambda () (ambeval-exp '(factorial 5000)))) ;; .57 .01 .603
(reset-env!)

;; 2. Mixed syntactic analysis
;; (measure-run-time (lambda () (eval-exp '(factorial 1000)))) ;; .19 .01 .203
;; (measure-run-time (lambda () (eval-exp '(factorial 2000)))) ;; .35 0. .355
;; (measure-run-time (lambda () (eval-exp '(factorial 5000)))) ;; .89 .01 .911
;; (reset-env!)


;; Below are a couple of examples where an error is generated
;; in an applicative-order language.

(ambeval-exp '(define (try a b)
                (if (= a 0) 1 b)))

(ambeval-exp '(try 0 (/ 1 0))) ; Division by zero signalled by /
(reset-env!)

;; 'unless' as a procedure (rather than a special form)
(ambeval-exp '(define (unless condition usual-value exceptional-value)
                (if condition exceptional-value usual-value)))

(ambeval-exp '(let ((a 0) (b 0))
                (unless (= b 0)
                  (/ a b)
                  (begin (display "exception: returning 0")
                         0)))) ; Division by zero signalled by /
(reset-env!)


;; ramb (searches alternatives in a random order)
;; Run in the driver loop
;;
;; (ramb 1 2 3 4 5) ; 5
;; try-again        ; 3
;; try-again        ; 1
;; try-again        ; 2
;; try-again        ; 4
;; try-again        ; There are no more values
