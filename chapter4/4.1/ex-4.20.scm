;; Exercise 4.20
;;
;; Because internal definitions look sequential but are
;; actually simultaneous, some people prefer to avoid them
;; entirely, and use the special form 'letrec' instead.
;;
;; Letrec looks like 'let', so it is not surprising that
;; the variables it binds are bound simultaneously and
;; have the same scope as each other.
;;
;; The sample procedure f above can be written without
;; internal definitions, but with exactly the same meaning, as

(define (f x)
  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    <rest of body of f>))

;; Letrec expressions, which have the form
;;
;; (letrec ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)
;;
;; are a variation on 'let' in which the expressions <exp-k>
;; that provide the initial values for the variables <var-k>
;; are evaluated in an environment that includes
;; all the letrec bindings.
;;
;; This permits recursion in the bindings, such as
;; the mutual recursion of even? and odd? in the example above,
;; or the evaluation of 10 factorial with

(letrec ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
  (fact 10))

;; Value: 3628800

;; a. Implement letrec as a derived expression, by transforming
;;    a 'letrec' expression into a 'let' expression as shown in
;;    the text above or in exercise 4.18.
;;    That is, the 'letrec' variables should be created with
;;    a 'let' and then be assigned their values with 'set!'.
;;
;; b. Louis Reasoner is confused by all this fuss about
;;    internal definitions. The way he sees it,
;;    if you don't like to use 'define' inside a procedure,
;;    you can just use 'let'.
;;    Illustrate what is loose about his reasoning by drawing
;;    an environment diagram that shows the environment in which
;;    the <rest of body of f> is evaluated during evaluation of
;;    the expression (f 5), with f defined as in this exercise.
;;
;;    Draw an environment diagram for the same evaluation, but
;;    with 'let' in place of 'letrec' in the definition of f.


;; a. See: eval-letrec.scm


;; b. The function won't work with 'let' due to its being unable
;;    to find the definitions for the inner variables, that
;;    are not present in the enclosing environments.
;;    (Exception: the availability of built-in procedures can
;;                make it work, but should not be relied upon.)
;;
;;
;; b. letrec
;;
;; Consider the following implementation of f:

(define (f x)
  (letrec ((even? (lambda (n)
                    (if (= n 0) true (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0) false (even? (- n 1))))))
    (even? x)))

;; (f 5) ; false
;;
;; During the evaluation of letrec the following form

(letrec ((even? (lambda (n)
                  (if (= n 0) true (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0) false (even? (- n 1))))))
  (even? 5))

;; expands to

((lambda (even? odd?)
   (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
   (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
   (even? 5))
 (quote *unassigned*) (quote *unassigned*))

;; During the external lambda invocation the variables even? and odd?
;; are both initialized with '*unassigned* and become part of
;; the enclosing environment where the internal lambdas get executed,
;; which makes the variables accessible to them.

;; Below is a diagram of the evaluation of (f 5) with letrec:
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env      f---↓
;;               |
;; --------------|-------------------------------------------------------------------
;;               |    ↑                    ↑
;;               ↓    |           _________|_________________________________________
;;             |x|x|--↑          |
;;              |              E1|x:5
;;              ↓                |___________________________________________________
;;    parameters: x                     ↑                              ↑
;;          body: (letrec ...)          |                      ________|_____________
;;                                      |                     |
;;                                      |                   E2|even?:'*unassigned*
;;                               |x|x|--↑                     |odd?:'*unassigned*
;;                                |                           |______________________
;;                                ↓
;;                      parameters: even? odd?                   (set! even? (lambda ..))
;;                            body: (set! even? (lambda ..))     (set! odd? (lambda ..))
;;                                  (set! odd? (lambda ..))      (even? 5)
;;                                  (even? 5)



;; b. (f 5) - let

(define (f x)
  (let ((even? (lambda (n)
                 (if (= n 0) true (odd? (- n 1)))))
        (odd? (lambda (n)
                (if (= n 0) false (even? (- n 1))))))
    (even? x)))

;; (f 5) ; false | Works only due to the built-in odd? and even?.

;; expands to

((lambda (even? odd?) (even? 5))
 (lambda (n) (if (= n 0) true (odd? (- n 1))))
 (lambda (n) (if (= n 0) false (even? (- n 1)))))

;; or, withouth relying on the built-in odd? and even?
;;
;; ((lambda (even1? odd1?) (even1? 5))
;;  (lambda (n) (if (= n 0) true (odd1? (- n 1))))
;;  (lambda (n) (if (= n 0) false (even1? (- n 1)))))
;;
;; which results to Unbound variable: odd1?

;; Here, all three lambdas share the same environment,
;; in which neither odd? (odd1?) nor even? (even1?) are defined,
;; making the last two lambdas, that are applied as arguments,
;; unable to find their definitions and fail.

;; Below is a diagram of the evaluation of (f 5) with let:
;;
;; -----------------------------------------------------------------------------------------
;;
;;  Global
;;  Env      f---↓
;;               |
;; --------------|--------------------------------------------------------------------------
;;               |    ↑                    ↑
;;               ↓    |           _________|________________________________________________
;;             |x|x|--↑          |
;;              |              E1|x:5
;;              ↓                |__________________________________________________________
;;    parameters: x                     ↑                    ↑                        ↑  
;;          body: (let ...)             |            ________|___________        _____|_____
;;                                      |           |                    |      |
;;                                      |         E2|even?:(lambda ...)  |    E3|n:5
;;                               |x|x|--↑           |odd?:(lambda ...)   |      |
;;                                |                 |____________________|      |___________
;;                                ↓                                   ↑
;;                      parameters: even? odd?         (even? 5)      |            (if (= n 0)
;;                            body: (even? 5)                         |                true
;;                                                                    |                (odd? (- n 1)))
;;                                                             |x|x|--↑
;;                                                              |
;;                                                              ↓
;;                                                    parameters: n
;;                                                          body: (if (= n 0)
;;                                                                    true
;;                                                                    (odd? (- n 1)))

;; As shown above, the body of the inner procedure even?
;; is evaluated in the environment E3, whose enclosing
;; environment is E1, and the variable odd? is not defined
;; in this chain of frames.
