;; Eval letrec
;;
;; Extends the given evaluator with letrec expressions.
;;
;; Letrec expressions, which have the form
;; 
;; (letrec ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)
;;
;; are a variation on let in which the expressions <expk>
;; that provide the initial values for the variables <vark>
;; are evaluated in an environment that includes
;; all the letrec bindings.
;;
;;
;; The form is implemented as a derived expression,
;; by transforming itself into a 'let' expression:
;;
;; (let ((<var1> '*unassigned*)
;;       ...
;;       (<varn> '*unassigned*))
;;   (set! <var1> <exp1>)
;;   ...
;;   (set! <varn> <expn>)
;;   <body>)
;;
;;
;; For example:
;;
;; (letrec ((fact
;;           (lambda (n)
;;             (if (= n 1)
;;                 1
;;                 (* n (fact (- n 1)))))))
;;   (fact 10))
;;
;; would be transformed into
;;
;; (let ((fact '*unassigned*))
;;   (set! fact (lambda (n)
;;                (if (= n 1)
;;                    1
;;                    (* n (fact (- n 1))))))
;;   (fact 10))
;;
;; where *unassigned* is a special symbol that causes
;; looking up a variable to signal an error if an attempt
;; is made to use the value of the not-yet-assigned variable.


(load "list-utils.scm")

(define (install-eval-letrec evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))
    
    (define (eval-letrec exp env)
      (_eval (letrec->let exp) env))

    (define (analyze-letrec exp)
      (_analyze (letrec->let exp)))

    (define (letrec->let exp)
      (let* ((vars-exps (decouple-pairs (letrec-var-exp-pairs exp)))
             (vars (car vars-exps))
             (exps (cadr vars-exps)))
        (make-let
         (map var->unassigned-pair vars)
         (append (map var-exp->make-set! vars exps)
                 (letrec-body exp)))))

    (define (var->unassigned-pair var)
      (list var (quote '*unassigned*)))

    (define (var-exp->make-set! var exp)
      (make-set! var (list exp)))

    (define (letrec-var-exp-pairs exp) (cadr exp))
    (define (letrec-body exp) (cddr exp))

    ;; Dependency constructors
    (define (make-let . args)
      (apply (get-constructor 'make-let) args))
    (define (make-set! . args)
      (apply (get-constructor 'make-set!) args))

    (extend-eval 'letrec eval-letrec)
    (extend-analyze 'letrec analyze-letrec)))
