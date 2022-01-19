;; Eval While
;;
;; 1. Syntax
;;
;; (while <predicate> <body>)
;;
;; Repeatedly executes body while test <predicate> is true.
;; Presumes some side-effect will cause <predicate> to become false.
;;
;;
;; 2. Design
;;
;; The form is implemented as a derived expression that unfolds
;; to a named 'let':
;;
;; (while <predicate> <body>)
;;
;; becomes
;;
;; (let loop ()
;;   (if <predicate>
;;       (begin
;;         <body>
;;         (loop))))
;;
;;
;; 3. Example
;;
;; (define (factorial x)
;;   (let ((n 1)
;;         (i x))
;;     (while (> i 0)
;;            (set! n (* n i))
;;            (set! i (- i 1)))
;;     n))
;;
;; becomes
;;
;; (define (factorial x)
;;   (let ((n 1)
;;         (i x))
;;     (let loop ()
;;       (if (> i 0)
;;           (begin
;;             (set! n (* n i))
;;             (set! i (- i 1))
;;             (loop))))
;;     n))
;;
;; (factorial 1) ; 1
;; (factorial 2) ; 2
;; (factorial 3) ; 6
;; (factorial 4) ; 24
;; (factorial 5) ; 120

(load "list-utils.scm")

(define (install-eval-while evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))

    (define (eval-while exp env)
      (_eval (while->named-let exp) env))

    (define (analyze-while exp)
      (_analyze (while->named-let exp)))

    (define (while->named-let exp)
      (let ((name 'loop))
        (make-named-let
         name
         '()
         (list
          (make-if-altless
           (while-predicate exp)
           (make-begin
            (append (while-body exp)
                    (list (list name)))))))))
    
    (define (while-predicate exp) (cadr exp))
    (define (while-body exp) (cddr exp))

    ;; Dependency constructors
    (define (make-named-let . args)
      (apply (get-constructor 'make-named-let) args))
    (define (make-if-altless . args)
      (apply (get-constructor 'make-if-altless) args))
    (define (make-begin . args)
      (apply (get-constructor 'make-begin) args))

    (extend-eval 'while eval-while)
    (extend-analyze 'while analyze-while)))
