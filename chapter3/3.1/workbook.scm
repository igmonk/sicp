;; 3.1 Assignment and Local State

;; Local State Variables

;; Modelling of the situation of withdrawing money from a bank account
;;
;; To implement 'withdraw', we can use a variable 'balance' to indicate
;; the balance of money in the account and define 'withdraw' as a procedure
;; that accesses 'balance'.
;; The withdraw procedure checks to see if 'balance' is at least as large as
;; the requested amount. If so, 'withdraw' decrements 'balance' by amount and
;; returns the new value of 'balance'.
;; Otherwise, 'withdraw' returns the 'Insufficient funds message'.

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; Decrementing balance is accomplished by the expression
;;
;; (set! balance (- balance amount))
;;
;; This uses the set! special form, whose syntax is
;;
;; (set! <name> <new-value>)
;;
;; Here <name> is a symbol and <new-value> is any expression.
;; Set! changes <name> so that its value is the result obtained by
;; evaluating <new-value>. 


;; Although withdraw works as desired, the variable balance presents a problem.
;; As specified above, balance is a name defined in the global environment and
;; is freely accessible to be examined or modified by any procedure.
;;
;; We can make 'balance' internal to 'withdraw' by rewriting the definition
;; as follows:

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; In programming-language jargon, the variable 'balance' is said to be
;; 'encapsulated' within the 'new-withdraw' procedure.
;;
;; Encapsulation reflects the general system-design principle known as
;; the 'hiding principle':
;; One can make a system more modular and robust by protecting parts of
;; the system from each other; that is, by providing information access
;; only to those parts of the system that have a 'need to know'.


;; The following procedure, make-withdraw, creates 'withdrawal processors'.
;; The formal parameter 'balance' in 'make-withdraw' specifies
;; the initial amount of money in the account.

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;; (define w1 (make-withdraw 100))
;; (define w2 (make-withdraw 100))

;; (w1 50) ; 50
;; (w2 70) ; 30
;; (w2 40) ; "insufficient funds"
;; (w1 40) ; 10

;; Observe that W1 and W2 are completely independent objects,
;; each with its own local state variable balance.
;; Withdrawals from one do not affect the other.


;; We can also create objects that handle deposits as well as withdrawals,
;; and thus we can represent simple bank accounts.
;; Here is a procedure that returns a 'bank-account object' with
;; a specified initial balance:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Each call to make-account sets up an environment with
;; a local state variable 'balance'. Within this environment,
;; 'make-account' defines procedures 'deposit' and 'withdraw' that
;; access 'balance' and an additional procedure 'dispatch' that
;; takes a 'message' as input and returns one of the two local procedures.

;; (define acc (make-account 100))

;; ((acc 'withdraw) 50) ; 50
;; ((acc 'withdraw) 60) ; "Insufficient funds"
;; ((acc 'deposit) 40)  ; 90
;; ((acc 'withdraw) 60) ; 30

;; Another call to 'make-account' will produce a completely separate
;; account object, which maintains its own local 'balance'.


;; The Benefits of Introducing Assignment

;; One common way to implement rand-update is to use the rule that
;; x is updated to ax + b modulo m, where a, b, and m are appropriately
;; chosen integers.
;; Chapter 3 of Knuth 1981 includes an extensive discussion of
;; techniques for generating sequences of random numbers and
;; establishing their statistical properties.
;; Notice that the rand-update procedure computes a mathematical function:
;; Given the same input twice, it produces the same output.
;;
;; Load the support code from UC Berkeley:

(load "../ch3support.scm")

;; We can implement 'rand' as a procedure with a local state variable x
;; that is initialized to some fixed value 'random-init'.
;;
;; Each call to 'rand' computes 'rand-update' of the current value of x,
;; returns this as the random number, and also stores this as
;; the new value of x.

(define random-init (random 100))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; (rand) ; 23
;; (rand) ; 12
;; (rand) ; 96

;; Of course, we could generate the same sequence of random numbers
;; without using assignment by simply calling 'rand-update' directly.
;;
;; However, this would mean that any part of our program that used
;; random numbers would have to explicitly remember the current value
;; of x to be passed as an argument to 'rand-update'.


;; The Costs of Introducing Assignment

;; A language that supports the concept that 'equals can be substituted for equals'
;; in an expresssion without changing the value of the expression is said to be
;; 'referentially transparent'.
;;
;; Referential transparency is violated when we include 'set!'
;; in our computer language. This makes it tricky to determine when we can simplify
;; expressions by substituting equivalent expressions. Consequently,
;; reasoning about programs that use assignment becomes drastically more difficult.


;; Pitfalls of imperative programming

;; In contrast to functional programming, programming that makes extensive use of
;; assignment is known as 'imperative programming'.
;;
;; In addition to raising complications about computational models,
;; programs written in imperative style are susceptible to bugs that
;; cannot occur in functional programs.
;;
;; Recall the iterative factorial program from section 1.2.1:

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; (factorial 5) ; 120

;; Instead of passing arguments in the internal iterative loop,
;; we could adopt a more imperative style by using explicit assignment
;; to update the values of the variables 'product' and 'counter':

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;; (factorial 5) ; 120

;; This does not change the results produced by the program, but
;; it does introduce a subtle trap.
;;
;; How do we decide the order of the assignments>
;; As it happens, the program is correct as written.
;;
;; But writing the assignments in the opposite order
;;
;; (set! counter (+ counter 1))
;; (set! product (* counter product))
;;
;; would have produced a different, incorrect result.

;; In general, programming with assignment forces us to carefully consider
;; the relative orders of the assignments to make sure that each statement
;; is using the correct version of the variables that have been changed.
;;
;; This issue simply does not arise in functional programs.
;; The complexity of imperative programs becomes even worse if
;; we consider applications in which several processes execute concurrently.
