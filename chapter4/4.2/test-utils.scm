;; Test utilities

(load "../../chapter3/3.3/table-obj-2d.scm")

(load "list-utils.scm")
(load "environment.scm")
(load "procedure.scm")
(load "thunk.scm")
(load "evaluator.scm")

;; Load evaluator extensions
(load "eval-quote.scm")
(load "eval-assignment.scm")
(load "eval-definition.scm")
(load "eval-if.scm")
(load "eval-lambda.scm")
(load "eval-begin.scm")
(load "eval-cond.scm")
(load "eval-and.scm")
(load "eval-or.scm")
(load "eval-let.scm")
(load "eval-let-star.scm")
(load "eval-while.scm")
(load "eval-undefine.scm")
(load "eval-letrec.scm")
(load "eval-unless.scm")


;; Create a new evaluator and extend it with the necessary forms
(define (create-new-evaluator)
  (let ((evaluator (make-evaluator (make-table))))
    (extend-evaluator evaluator)
    evaluator))

;; Extend the evaluator with the necessary forms
(define (extend-evaluator evaluator)
  (install-eval-quote evaluator)
  (install-eval-assignment evaluator)
  (install-eval-definition evaluator)
  (install-eval-if evaluator)
  (install-eval-lambda evaluator)
  (install-eval-begin evaluator)
  (install-eval-cond evaluator)
  (install-eval-and evaluator)
  (install-eval-and-d evaluator)
  (install-eval-or evaluator)
  (install-eval-or-d evaluator)
  (install-eval-let evaluator)
  (install-eval-let* evaluator)
  (install-eval-while evaluator)
  (install-eval-undefine evaluator)
  (install-eval-letrec evaluator)
  (install-eval-unless evaluator))

;; Setup environment
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             (make-environment))))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;; Setup some primitive procedures

(define primitive-procedures
  (list (list '_car car)
        (list '_cdr cdr)
        (list '_cons cons)
        (list 'null? null?)
        (list '= =)
        (list 'eq? eq?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list 'list list)
        (list 'newline newline)
        (list 'display display)
        ;; <more primitives>
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


;; REPL

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop env actual-value)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop env actual-value))

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display str)
  (newline))

(define (lazy-list? object)
  (tagged-list? object 'lazy-list))

;; Based on the definition of cons:
;;
;; (define (cons x y)
;;   (_cons 'lazy-list (lambda (m) (m x y)))))

;; Equals to (eval (car pair))
(define (lazy-list-car-value ll)
  (lookup-variable-value 'x (procedure-environment (cdr ll))))

;; Equals to (eval (cdr pair))
(define (lazy-list-cdr-value ll)
  (lookup-variable-value 'y (procedure-environment (cdr ll))))

;; The procedure identifies whether the given stream is
;; an implicitly defined one (without generating procedure), for ex.:
;;
;; (define ones (cons 1 ones)) ; * normal-order evaluation
;;
;; For that, we need to compare the environments of the procedures
;; accessing the current and following items of the given lazy list.
;;
;; The procedure is useful to avoid the infinite printing loop,
;; since as soon as the first two elements of an implicitly defined
;; lazy list have been forced (= transformed to evaluated thunks),
;; a simple check for evaluated-thunk? will not suffice for the
;; following reason:
;;
;; 1. car of the lazy-list will refer to an evaluated thunk that
;;        keeps the actual value (1 in case of ones)
;; 2. cdr of the lazy-list will refer to an evaluated thunk that
;;        points to the lazy list, whose car is described in 1.,
;;        and whose cdr points to the evaluated thunk described in 2.,
;;        leading to the infinite recursion
;;
;; Due to the inner structure of lazy lists, this infinite recursion
;; can be detected by comparing the environments of the procedures
;; representing the successive items of the list.
(define (implicit-infinite-list? ll ll-rest)
  (eq? (procedure-environment (cdr ll))
       (procedure-environment (cdr ll-rest))))

(define (print-lazy-list lazy-list)
  (define (inner ll)
    (when (not (null? ll))
      (let ((first (lazy-list-car-value ll)))
        (if (evaluated-thunk? first)
            (let ((rest (lazy-list-cdr-value ll)))
              (display (thunk-value first))
              (display " ")
              (if (evaluated-thunk? rest)
                  (let ((ll-rest (thunk-value rest)))
                    (if (implicit-infinite-list? ll ll-rest)
                        (display "...")
                        (inner ll-rest)))
                  (display "...")))
            (display "...")))))
  (display "{")
  (inner lazy-list)
  (display "}"))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-list? object)
         (print-lazy-list object))
        (else
         (display object))))


;; MIT Scheme - 15.6 Machine Time
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html#Machine-Time

(define (measure-run-time fn)
  (with-timings
   (lambda () (fn))
   (lambda (run-time gc-time real-time)
     (write (internal-time/ticks->seconds run-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds gc-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds real-time))
     (newline))))
