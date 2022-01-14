;; Eval Let
;;
;; Extends the given evaluator with let expressions.
;;
;; let is a derived expression, which is implemented
;; as a combination as shown below.
;;
;; (let ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)
;;
;; is equivalent to
;;
;; ((lambda (<var1> ... <varn>)
;;    <body>)
;;  <exp1>
;;  ...
;;  <expn>)
;;
;; 
;; Named let (ex. 4.8)
;;
;; "Named let" is a variant of let that has the form
;;
;; (let <var> <bindings> <body>)
;;
;; The <bindings> and <body> are just as in ordinary let,
;; except that <var> is bound within <body> to a procedure
;; whose body is <body> and whose parameters are the variables
;; in the <bindings>.
;;
;; Thus, one can repeatedly execute the <body> by invoking
;; the procedure named <var>.
;;
;; For example, the iterative Fibonacci procedure (section 1.2.2)
;; can be rewritten using named let as follows:
;;
;; (define (fib n)
;;   (let fib-iter ((a 1)
;;                  (b 0)
;;                  (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))
;;
;;
;; The evaluator is to transform the inner 'let' to
;; the following sequence of expressions:
;;
;; (begin
;;   (define (fib-iter a b count)
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1))))
;;   (let ((a 1)
;;         (b 0)
;;         (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))
;;
;; Being part of the definition 'fib', the resulting sequence
;; is to be executed in its own environment, where fib-iter
;; is assigned to the inner recursive function and is available
;; not only within its inner code block, but also within
;; the subsequent let-block.

(load "list-utils.scm")

(define (install-eval-let evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor))
        (get-constructor (evaluator 'get-constructor)))

    (define (eval-let exp env)
      (_eval (let->combination exp) env))

    (define (analyze-let exp)
      (_analyze (let->combination exp)))

    (define (let->combination exp)
      (if (symbol? (cadr exp)) ; if named let
          (named-let->combination exp)
          (ordinary-let->combination exp)))

    (define (named-let->combination exp)
      (let ((name (cadr exp))
            (var-exp-pairs (caddr exp))
            (body (cdddr exp)))
        (let ((params (car (decouple-pairs var-exp-pairs))))
          (make-begin
           (list (make-define (cons name params) body)
                 (make-let var-exp-pairs body))))))
    
    (define (ordinary-let->combination exp)
      (let ((var-exps (decouple-pairs (let-var-exp-pairs exp)))
            (body (let-body exp)))
        (append (list (make-lambda (car var-exps) body))
                (cadr var-exps))))

    (define (make-let var-exp-pairs body)
      (cons 'let (cons var-exp-pairs body)))
    (define (let-var-exp-pairs exp) (cadr exp))
    (define (let-body exp) (cddr exp))

    (define (make-named-let name var-exp-pairs body)
      (cons 'let (cons name (cons var-exp-pairs body))))

    ;; Dependency constructors
    (define (make-begin . args)
      (apply (get-constructor 'make-begin) args))
    (define (make-define . args)
      (apply (get-constructor 'make-define) args))
    (define (make-lambda . args)
      (apply (get-constructor 'make-lambda) args))

    (extend-eval 'let eval-let)
    (extend-analyze 'let analyze-let)
    (def-constructor 'make-let make-let)
    (def-constructor 'make-named-let make-named-let)))
