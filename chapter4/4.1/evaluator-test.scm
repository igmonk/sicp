;; Evaluator Test

(load "test-utils.scm")

;; Create the global environment
(define env (setup-environment))
(define (reset-env!) (set! env (setup-environment)) 'ok)

;; Create an evaluator
(define evaluator (create-new-evaluator))
(define (eval-exp exp) ((evaluator '_eval) exp env))

;; Run REPL
;; (driver-loop env (evaluator '_eval))


;; Test: self-evaluating
(eval-exp '100001) ; 100001
(eval-exp "hello") ; "hello"


;; Test: variable
(eval-exp 'car)   ; (primitive #[compiled-procedure ...
(eval-exp 'true)  ; true
(eval-exp 'false) ; false


;; Test: quoted
(eval-exp '(quote test1)) ; test1
(eval-exp '(quote test2)) ; test2


;; Test: definition
(eval-exp '(define x 1)) ; ok
(eval-exp 'x)            ; 1
(eval-exp '(define x 2)) ; ok
(eval-exp 'x)            ; 2
(reset-env!)


;; Test: assignment
(eval-exp 'x)            ; Unbound variable x
(eval-exp '(define x 1)) ; ok
(eval-exp 'x)            ; 1
(eval-exp '(set! x 2))   ; ok
(eval-exp 'x)            ; 2
(reset-env!)


;; Test: undefine
(eval-exp '(let ((x 100))
             (let ((y 200))
               (define x y)
               (undefine x)
               x))) ; 100


;; Test: if
(eval-exp '(if true 1 2))  ; 1
(eval-exp '(if true 2 1))  ; 2
(eval-exp '(if false 3 4)) ; 4
(eval-exp '(if false 4 3)) ; 3
(eval-exp '(if true 5))    ; 5
(eval-exp '(if false 5))   ; false


;; Test: unless
(eval-exp '(unless true 1 2))  ; 2
(eval-exp '(unless true 2 1))  ; 1
(eval-exp '(unless false 3 4)) ; 3
(eval-exp '(unless false 4 3)) ; 4
(eval-exp '(unless true 5))    ; false
(eval-exp '(unless false 5))   ; 5


;; Test: begin
(eval-exp '(begin 1 2 3)) ; 3
(eval-exp '(begin 4 5 6)) ; 6

(eval-exp '(begin 1 2 (+ 3 4 5))) ; 12
(eval-exp '(begin 1 2 (* 3 4 5))) ; 60


;; Test: cond
(eval-exp '(cond ((= 1 1) 100)
                 ((= 1 2) 200)
                 (else 300))) ; 100

(eval-exp '(cond ((= 1 2) 100)
                 ((= 1 1) 200)
                 (else 300))) ; 200

(eval-exp '(cond ((= 1 2) 100)
                 ((= 1 3) 200)
                 (else 300))) ; 300

(eval-exp '(cond ((cons 1 2) => car)
                 (else 'wrong))) ; 1


;; Test: and
(eval-exp '(and))                 ; true
(eval-exp '(and true true true))  ; true
(eval-exp '(and true true false)) ; false
(eval-exp '(and true true 100))   ; 100
(eval-exp '(and 100 200 300))     ; 300
(eval-exp '(and false true true)) ; false


;; Test: and (derived)
(eval-exp '(and-d))                 ; true
(eval-exp '(and-d true true true))  ; true
(eval-exp '(and-d true true false)) ; false
(eval-exp '(and-d true true 100))   ; 100
(eval-exp '(and-d 100 200 300))     ; 300
(eval-exp '(and-d false true true)) ; false


;; Test: or
(eval-exp '(or))                 ; false
(eval-exp '(or true true true))  ; true
(eval-exp '(or true true false)) ; true
(eval-exp '(or true true 100))   ; true
(eval-exp '(or 100 200 300))     ; 100
(eval-exp '(or false true true)) ; true


;; Test: or (derived)
(eval-exp '(or-d))                 ; false
(eval-exp '(or-d true true true))  ; true
(eval-exp '(or-d true true false)) ; true
(eval-exp '(or-d true true 100))   ; true
(eval-exp '(or-d 100 200 300))     ; 100
(eval-exp '(or-d false true true)) ; true


;; Test: lambda
(eval-exp '(lambda (x y) (cons x y)))       ; (procedure (x y ((cons x y)) ...
(eval-exp '((lambda (x y) (cons x y)) 1 2)) ; (1 . 2)


;; Test: let
(eval-exp '(let ((x 1) (y 2)) (cons x y)))     ; (1 . 2)
(eval-exp '(let ((id (lambda (x) x))) (id 1))) ; 1
(eval-exp '(let ((id (lambda (x) x))) (id 2))) ; 2


;; Test: let (nested)
(eval-exp '(let ((x 3))
             (let ((y (+ x 2)))
               (let ((z (+ x y 5)))
                 (* x z))))) ; 39


;; Test: let (named)
(eval-exp '(define (fib n)
             (let fib-iter ((a 1)
                            (b 0)
                            (count n))
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1))))))

(eval-exp '(fib 1)) ; 1
(eval-exp '(fib 2)) ; 1
(eval-exp '(fib 3)) ; 2
(eval-exp '(fib 4)) ; 3
(eval-exp '(fib 5)) ; 5
(eval-exp '(fib 6)) ; 8
(eval-exp '(fib 7)) ; 13
(eval-exp '(fib 8)) ; 21
(eval-exp '(fib 9)) ; 34
(reset-env!)


;; Test: let*
(eval-exp '(let* ((x 3)
                  (y (+ x 2))
                  (z (+ x y 5)))
             (* x z))) ; 39

(eval-exp '(let* ((x 3)) (* x x))) ; 9


;; Test: internal definitions (definitions come first)
(eval-exp '(define (f x)
             (define (even? n)
               (if (= n 0)
                   true
                   (odd? (- n 1))))
             (define (odd? n)
               (if (= n 0)
                   false
                   (even? (- n 1))))
             (even? x)))

(eval-exp '(f 5)) ; false
(eval-exp '(f 6)) ; true
(reset-env!)


;; Test: internal definitions (procedure calls come first)
(eval-exp '(define (f x)
             (even? x)
             (define (even? n)
               (if (= n 0)
                   true
                   (odd? (- n 1))))
             (define (odd? n)
               (if (= n 0)
                   false
                   (even? (- n 1))))))

(eval-exp '(f 5)) ; false (if scan-out-defines is on. Otherwise: Unbound variable even?)
(eval-exp '(f 6)) ; true (if scan-out-defines is on. Otherwise: Unbound variable even?)
(reset-env!)


;; Test: letrec
(eval-exp '(letrec ((fact
                     (lambda (n)
                       (if (= n 1)
                           1
                           (* n (fact (- n 1)))))))
             (fact 10))) ; 3628800

(eval-exp '(letrec ((even?
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
(eval-exp '(define (factorial x)
             (let ((n 1)
                   (i x))
               (while (> i 0)
                      (set! n (* n i))
                      (set! i (- i 1)))
               n)))

(eval-exp '(factorial 1)) ; 1
(eval-exp '(factorial 2)) ; 2
(eval-exp '(factorial 3)) ; 6
(eval-exp '(factorial 4)) ; 24
(eval-exp '(factorial 5)) ; 120
(reset-env!)


;; Test: application (primitive procedure)
(eval-exp '(not true))         ; false
(eval-exp '(not false))        ; true
(eval-exp '(= 100 100))        ; true
(eval-exp '(= 100 200))        ; false
(eval-exp '(not (= 100 100)))  ; false
(eval-exp '(not (= 100 200)))  ; true
(eval-exp '(eq? 'abc 'abc))    ; true
(eval-exp '(eq? 'abc 'def))    ; false
(eval-exp '(cons 1 2))         ; (1 . 2)
(eval-exp '(car (cons 1 2)))   ; 1
(eval-exp '(cdr (cons 1 2)))   ; 2
(eval-exp '(null? '()))        ; true
(eval-exp '(null? (cons 1 2))) ; false


;; Test: application (compound procedure)
(eval-exp '(define (append x y)
             (if (null? x)
                 y
                 (cons (car x)
                       (append (cdr x) y))))) ; ok

(eval-exp 'append)                     ; (procedure (x y) ((if (null? x) ...
(eval-exp '(append '(a b c) '(d e f))) ; (a b c d e f)
(reset-env!)


;; Test: unassigned
(eval-exp '(define x '*unassigned*))
(eval-exp 'x) ; Unassigned variable x
(reset-env!)


;; Below are some experiments to compare the speed of the original
;; metacircular evaluator (evaluation is mixed with syntactic analysis)
;; with the version in the section 4.1.7 (syntactic analysis precedes
;; evaluation).

;; Test method
(eval-exp '(define (factorial n)
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
(measure-run-time (lambda () (eval-exp '(factorial 1000)))) ;; .1 0. .105
(measure-run-time (lambda () (eval-exp '(factorial 2000)))) ;; .18 0. .198
(measure-run-time (lambda () (eval-exp '(factorial 5000)))) ;; .45 .01 .474
(reset-env!)

;; 2. Mixed syntactic analysis
(measure-run-time (lambda () (eval-exp '(factorial 1000)))) ;; .19 .01 .203
(measure-run-time (lambda () (eval-exp '(factorial 2000)))) ;; .35 0. .355
(measure-run-time (lambda () (eval-exp '(factorial 5000)))) ;; .89 .01 .911
(reset-env!)


;; Below are a couple of examples where an error is generated
;; in an applicative-order language.

(eval-exp '(define (try a b)
             (if (= a 0) 1 b)))

(eval-exp '(try 0 (/ 1 0))) ; Division by zero signalled by /
(reset-env!)

;; 'unless' as a procedure (rather than a special form)
(eval-exp '(define (unless condition usual-value exceptional-value)
             (if condition exceptional-value usual-value)))

(eval-exp '(let ((a 0) (b 0))
             (unless (= b 0)
               (/ a b)
               (begin (display "exception: returning 0")
                      0)))) ; Division by zero signalled by /
(reset-env!)
