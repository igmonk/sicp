;; Test utilities

(load "../../common.scm")
(load "../../chapter3/3.3/table-obj-2d.scm")

(load "environment.scm")
(load "procedure.scm")
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
(load "eval-xor.scm")
(load "eval-let.scm")
(load "eval-let-star.scm")
(load "eval-while.scm")
(load "eval-undefine.scm")
(load "eval-letrec.scm")
(load "eval-unless.scm")
(load "eval-amb.scm")
(load "eval-ramb.scm")
(load "eval-passignment.scm")
(load "eval-if-fail.scm")
(load "eval-require.scm")


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
  (install-eval-xor evaluator)
  (install-eval-let evaluator)
  (install-eval-let* evaluator)
  (install-eval-while evaluator)
  (install-eval-undefine evaluator)
  (install-eval-letrec evaluator)
  (install-eval-unless evaluator)
  (install-eval-amb evaluator)
  (install-eval-ramb evaluator)
  (install-eval-passignment evaluator)
  (install-eval-if-fail evaluator)
  (install-eval-require evaluator))

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
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
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
        (list '>= >=)
        (list '<= <=)
        (list 'list list)
        (list 'member member)
        (list 'prime? prime?)
        (list 'square square)
        (list 'sqrt sqrt)
        (list 'integer? integer?)
        (list 'abs abs)
        (list 'memq memq)
        (list 'member member)
        (list 'list-ref list-ref)
        (list 'length length)
        (list 'random random)
        (list 'min min)
        (list 'max max)
        (list 'display display)
        (list 'even? even?)
        (list 'odd? odd?)
        ;; <more primitives>
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


;; REPL

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop env ambeval-fn)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval-fn input
                        env
                        ;; ambeval success
                        (lambda (val next-alternative)
                          (announce-output output-prompt)
                          (user-print val)
                          (internal-loop next-alternative))
                        ;; ambeval failure
                        (lambda ()
                          (announce-output ";;; There are no more values of")
                          (user-print input)
                          (driver-loop env ambeval-fn)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop env ambeval-fn))))

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display str)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


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
