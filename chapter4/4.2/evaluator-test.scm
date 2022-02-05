;; Evaluator Test

(load "test-utils.scm")

;; Create the global environment
(define env (setup-environment))
(define (reset-env!) (set! env (setup-environment)) 'ok)

;; Create an evaluator
(define evaluator (create-new-evaluator))
(define (eval-exp exp) ((evaluator '_eval) exp env))

;; Run REPL
;; (driver-loop env (evaluator 'actual-value))


;; Test: self-evaluating
(eval-exp '100001) ; 100001
(eval-exp "hello") ; "hello"


;; Test: variable
(eval-exp '_car)   ; (primitive #[compiled-procedure ...
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

;; Upward-compatible extension (ex. 4.31):
;;   100
;;
;; Not an upward-compatible extension:
;;   100 if eval-sequence forces the final expression,
;;   (thunk[-memo] 100 #[compound-procedure ...]) otherwise.

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

(eval-exp '(cond ((_cons 1 2) => _car)
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
(eval-exp '(lambda (x y) (_cons x y)))       ; (procedure (x y ((cons x y)) ...
(eval-exp '((lambda (x y) (_cons x y)) 1 2)) ; (1 . 2)


;; Test: let
(eval-exp '(let ((x 1) (y 2)) (_cons x y))) ; (1 . 2)

(eval-exp '(let ((id (lambda (x) x))) (id 1)))
;; Upward-compatible extension (ex. 4.31):
;;   1
;;
;; Not an upward-compatible extension:
;;   1 if eval-sequence forces the final expression,
;;   (thunk[-memo] 1 #[compound-procedure ...]) otherwise.

(eval-exp '(let ((id (lambda (x) x))) (id 2)))
;; Upward-compatible extension (ex. 4.31):
;;   2
;;
;; Not an upward-compatible extension:
;;   2 if eval-sequence forces the final expression,
;;   (thunk[-memo] 2 #[compound-procedure ...]) otherwise.


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

;; The actual values are returned if eval-sequence forces thunks
;; and/or lazy evaluation is implemented as an upward-compatible
;; extension (ex. 4.31), otherwise
;; the result of calling 'fib' will be a thunk (thunk-memo).
;;
;; You may consider trying to run the expressions in the driver-loop:
;; (driver-loop env (evaluator 'actual-value))
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
(eval-exp '(not true))          ; false
(eval-exp '(not false))         ; true
(eval-exp '(= 100 100))         ; true
(eval-exp '(= 100 200))         ; false
(eval-exp '(not (= 100 100)))   ; false
(eval-exp '(not (= 100 200)))   ; true
(eval-exp '(eq? 'abc 'abc))     ; true
(eval-exp '(eq? 'abc 'def))     ; false
(eval-exp '(_cons 1 2))         ; (1 . 2)
(eval-exp '(_car (_cons 1 2)))  ; 1
(eval-exp '(_cdr (_cons 1 2)))  ; 2
(eval-exp '(null? '()))         ; true
(eval-exp '(null? (_cons 1 2))) ; false


;; Test: unassigned
(eval-exp '(define x '*unassigned*))
(eval-exp 'x) ; Unassigned variable x
(reset-env!)


;; Below are some experiments to compare the speed of
;; the normal-order evaluator based on:
;; 1. Defaults (applicative-order)
;; 2. Thunks (non-memoized)
;; 3. Memoized thunks
;;
;; Requires: upward-compatible extension.
;;
;; Time values:
;; - run-time  -> the elapsed run time
;; - gc-time   -> the amount of time spent in the garbage collector
;; - real-time -> the elapsed real time
;;
;; All three times are in ticks.
;; A tick is a unit of time that is unspecified here but
;; can be converted to and from seconds by supplied procedures.

;; 1. Default (applicative-order)
(eval-exp '(define (factorial n)
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n))))

(measure-run-time (lambda () (eval-exp '(factorial 100)))) ;; .03 0. .028
(measure-run-time (lambda () (eval-exp '(factorial 200)))) ;; .06 0. .055
(measure-run-time (lambda () (eval-exp '(factorial 500)))) ;; .11 0. .116
(reset-env!)

;; 2. Thunks (non-memoized)
(eval-exp '(define (factorial (n lazy))
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n))))

(measure-run-time (lambda () (eval-exp '(factorial 100)))) ;; .48 .01 .497
(measure-run-time (lambda () (eval-exp '(factorial 200)))) ;; 1.89 .01 1.919
(measure-run-time (lambda () (eval-exp '(factorial 500)))) ;; 11.78 .1 11.975
(reset-env!)

;; 3. Memoized thunks
(eval-exp '(define (factorial (n lazy-memo))
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n))))

(measure-run-time (lambda () (eval-exp '(factorial 100)))) ;; .03 0. .029
(measure-run-time (lambda () (eval-exp '(factorial 200)))) ;; .05 0. .051
(measure-run-time (lambda () (eval-exp '(factorial 500)))) ;; .11 0. .115
(reset-env!)


;; Below are a couple of examples where an error is generated
;; in an applicative-order language.
;;
;; Ex. 4.31 introduces changes to the normal-order evaluator to
;; behave in an upward-compatible manner:
;; the parameters syntax is extended with optional 'lazy' and 'lazy-memo',
;; which dictate the way the arguments are treated.


;; 1. Default (applicative-order)
(eval-exp '(define (try a b)
             (if (= a 0) 1 b)))

(eval-exp '(try 0 (/ 1 0))) ; Division by zero signalled by /
(reset-env!)


;; 2. Lazy
(eval-exp '(define (try a (b lazy))
             (if (= a 0) 1 b)))

(eval-exp '(try 0 (/ 1 0))) ; 1
(reset-env!)


;; 3. Lazy memo
(eval-exp '(define (try a (b lazy-memo))
             (if (= a 0) 1 b)))

(eval-exp '(try 0 (/ 1 0))) ; 1
(reset-env!)


;; 'unless' as a procedure (rather than a special form)
;;
;; Note: the special form 'unless' takes precedence over
;;       the procedure, as any special form does it.
(eval-exp '(define (unless condition usual-value exceptional-value)
             (if condition exceptional-value usual-value)))

(eval-exp '(let ((a 0) (b 0))
             (unless (= b 0)
               (/ a b)
               (begin (display "exception: returning 0")
                      0))))
;; exception: returning 0
;; ;Value: 0
;;
;; To make it throw an error, the special form 'unless'
;; must be removed, since in the evaluation process
;; any special form has a higher priority.

;; Recursive factorial based on 'unless' above.
(eval-exp '(define (factorial n)
             (unless (= n 1)
               (* n (factorial (- n 1)))
               1)))

(eval-exp '(factorial 10)) ; 3628800 (see ex. 4.25)
(reset-env!)


;; Streams as Lazy Lists
;;
;; In case of the extension is upward-compatible (ex. 4.31)
;; apply the following transformation to the parameters:
;; x -> (x lazy-memo)
;;
;; The evaluation result (value or thunk) depends on
;; the extention type (upward-compatible with lazy/memo or not).
;;
;; * The following expressions were fed to the evaluator
;;   that is not upward-compatible.
;; * After all the definitions has been performed, eval expressions
;;   in the driver loop to force thunks and inspect the content of
;;   the lists.

(eval-exp '(define (cons x y)
             (lambda (m) (m x y))))

(eval-exp '(define (car z) (z (lambda (p q) p))))
(eval-exp '(define (cdr z) (z (lambda (p q) q))))

(eval-exp '(define l1 (cons 1 (cons 2 (cons 3 '())))))
(eval-exp '(define (square x) (* x x)))

(eval-exp '(define (list-ref items n)
             (if (= n 0)
                 (car items)
                 (list-ref (cdr items) (- n 1)))))

(eval-exp '(define (map proc items)
             (if (null? items)
                 '()
                 (cons (proc (car items))
                       (map proc (cdr items))))))

(eval-exp '(define (scale-list items factor)
             (map (lambda (x) (* x factor))
                  items)))

(eval-exp '(define (add-lists list1 list2)
             (cond ((null? list1) list2)
                   ((null? list2) list1)
                   (else (cons (+ (car list1) (car list2))
                               (add-lists (cdr list1) (cdr list2)))))))

(eval-exp '(define ones (cons 1 ones)))
(eval-exp '(define integers (cons 1 (add-lists ones integers))))

;; Inspect the lists' contents
;;
;; (driver-loop env (evaluator 'actual-value))

;; (list-ref l1 0) ; 1
;; (list-ref l1 1) ; 2
;; (list-ref l1 2) ; 3

;; (list-ref (map square l1) 0) ; 1
;; (list-ref (map square l1) 1) ; 4
;; (list-ref (map square l1) 2) ; 9

;; (list-ref (scale-list l1 5) 0) ; 5
;; (list-ref (scale-list l1 5) 1) ; 10
;; (list-ref (scale-list l1 5) 2) ; 15

;; (list-ref (add-lists l1 l1) 0) ; 2
;; (list-ref (add-lists l1 l1) 1) ; 4
;; (list-ref (add-lists l1 l1) 2) ; 6

;; (list-ref ones 0) ; 1
;; (list-ref ones 1) ; 1
;; (list-ref ones 2) ; 1

;; (list-ref integers 0) ; 1
;; (list-ref integers 1) ; 2
;; (list-ref integers 2) ; 3
;; (list-ref integers 3) ; 4
;; (list-ref integers 4) ; 5


;; Reimplemented integral from section 3.5.4:

(eval-exp '(define (integral (integrand lazy-memo) initial-value dt)
             (define int
               (cons initial-value
                     (add-lists (scale-list integrand dt)
                                int)))
             int))

(eval-exp '(define (solve f y0 dt)
             (define y (integral dy y0 dt))
             (define dy (map f y))
             y))

;; Inspect the lists' contents
;;
;; (driver-loop env (evaluator 'actual-value))

;; (list-ref (solve (lambda (x) x) 1 0.001) 1000) ; 2.716923932235896


;; Test: quoted list (ex. 4.33)
;; Requires: cons, car and cdr (defined above).

(define (av exp) ((evaluator 'actual-value) exp env))

;; Alternatively, run in the driver loop:
;; (driver-loop env (evaluator 'actual-value))

;; Before the changes to eval-quote:
(av '(car '(a b c)))        ; Unknown procedure type -- APPLY (a b c)
(av '(car (quote (a b c)))) ; Unknown procedure type -- APPLY (a b c)

;; After the quoted lists support has been added to eval-quote:
(av '(car '(a b c)))        ; Unbound variable a
(av '(car (quote (a b c)))) ; Unbound variable a
(av '(car '(1 2 3)))        ; 1
(av '(car (quote (1 2 3)))) ; 1


;; The defintions for lazy-lists to make their display
;; possible (ex. 4.34):

(eval-exp '(define (cons x y)
             (_cons 'lazy-list (lambda (m) (m x y)))))

(eval-exp '(define (car z) ((_cdr z) (lambda (p q) p))))
(eval-exp '(define (cdr z) ((_cdr z) (lambda (p q) q))))

;; Try running the following in the in the driver loop.
;; Requires: cons, car and cdr;
;;           list-ref, add-lists;
;;           ones, integers.
(driver-loop env (evaluator 'actual-value))

ones     ; {...}
integers ; {...}

(list-ref integers 0) ; 1

ones     ; {...}
integers ; {1 ...}

(list-ref integers 1) ; 2

ones     ; {1 ...}
integers ; {1 2 ...}

(list-ref integers 2) ; 3

ones     ; {1 ...}   !infinitely prints without the check for implicit infinite list
integers ; {1 2 3 ...}

(reset-env!)


;; Test: application (compound procedure)
;; Requires: cons, car and cdr to be defined.
(eval-exp '(define (append x y)
             (if (null? x)
                 y
                 (cons (car x)
                       (append (cdr x) y))))) ; ok

(eval-exp 'append)                     ; (procedure (x y) ((if (null? x) ...
(eval-exp '(append '(a b c) '(d e f))) ; (lazy-list procedure ... )
(reset-env!)
