;; Evaluator Machine Tests
;;
;; To reset the environment use the reset-env! procedure
;; (defined in evaluator-operations.scm).

(load "evaluator-machine.scm")


;; Start the evaluator machine and run the driver loop:
(start ec-eval-machine)


;; self-evaluating
1000001 ; 1000001
"hello" ; hello


;; variable
car   ; (primitive #[compiled-procedure ...
cdr   ; (primitive #[compiled-procedure ...
true  ; true
false ; false


;; quoted
(quote test1) ; test1
(quote test2) ; test2
'test3        ; test3
'test4        ; test4


;; definition
(define x 1) ; ok
x            ; 1
(define x 2) ; ok
x            ; 2
(reset-env!)


;; assignment
x            ; Unbound variable x
(define x 1) ; ok
x            ; 1
(set! x 2)   ; ok
x            ; 2
(reset-env!)


;; if
(if true 1 2)  ; 1
(if true 2 1)  ; 2
(if false 3 4) ; 4
(if false 4 3) ; 3
(if true 5)    ; 5
(if false 5)   ; false


;; cond
(cond ((= 1 1) 100)
      ((= 1 2) 200)
      (else 300)) ; 100

(cond ((= 1 2) 100)
      ((= 1 1) 200)
      (else 300)) ; 200

(cond ((= 1 2) 100)
      ((= 1 3) 200)
      (else 300)) ; 300

;; Implemented only for the derived form:
;;
;; (cond ((cons 1 2) => car)
;;       (else 'wrong)) ; 1

;; Signal an error if the 'else' clause is not the last one:
;;
;; (cond ((= 1 2) 100)
;;       (else 200)
;;       ((= 1 3) 300)) ; else-clause-is-not-last


;; begin
(begin 1 2 3)         ; 3
(begin 4 5 6)         ; 6
(begin 1 2 (+ 3 4 5)) ; 12
(begin 1 2 (* 3 4 5)) ; 60


;; lambda
(lambda (x y) (cons x y))       ; (procedure (x y ((cons x y)) ...
((lambda (x y) (cons x y)) 1 2) ; (1 . 2)


;; let
(let ((x 1) (y 2)) (cons x y))     ; (1 . 2)
(let ((id (lambda (x) x))) (id 1)) ; 1
(let ((id (lambda (x) x))) (id 2)) ; 2


;; let (nested)
(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z)))) ; 39


;; let (named)
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(fib 1) ; 1
(fib 2) ; 1
(fib 3) ; 2
(fib 4) ; 3
(fib 5) ; 5
(fib 6) ; 8
(fib 7) ; 13
(fib 8) ; 21
(fib 9) ; 34

(reset-env!)


;; Compound procedure definition & application
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(a b c) '(d e f)) ; (a b c d e f)

(reset-env!)

;; Stack statistics tests

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; (total-pushes = 3 maximum-depth = 3)

(factorial 5) ; (total-pushes = 144 maximum-depth = 28)
