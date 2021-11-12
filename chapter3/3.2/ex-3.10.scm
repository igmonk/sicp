;; Exercise 3.10
;;
;; In the make-withdraw procedure, the local variable balance is created
;; as a parameter of make-withdraw.
;; We could also create the local state variable explicitly, using let,
;; as follows:
;;
;; (define (make-withdraw initial-amount)
;;   (let ((balance initial-amount))
;;     (lambda (amount)
;;       (if (>= balance amount)
;;           (begin (set! balance (- balance amount))
;;                  balance)
;;           "Insufficient funds"))))
;;
;; Recall from section 1.3.2 that 'let' is simply syntactic sugar for
;; a procedure call:
;;
;; (let ((<var> <exp>)) <body>)
;;
;; is interpreted as an alternate syntax for
;;
;; ((lambda (<var>) <body>) <exp>)
;;
;; Use the environment model to analyze this alternate version of make-withdraw,
;; drawing figures like the ones above to illustrate the interactions
;;
;; (define W1 (make-withdraw 100))
;;
;; (W1 50)
;;
;; (define W2 (make-withdraw 100))
;;
;; Show that the two versions of make-withdraw create objects
;; with the same behavior.
;;
;; How do the environment structures differ for the two versions?


;; Result of defining 'make-withdraw' in the global environment:
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env      make-withdraw---↓
;;                           |
;; --------------------------|-------------------------------------------------------
;;                           |    ↑
;;                           ↓    |
;;                         |x|x|--↑
;;                          |
;;                          ↓
;;                parameters: initial-amount
;;                body: (let ((balance initial-amount))
;;                        ...


;; Result of evaluating (define W1 (make-withdraw 100))
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env      W1---↓                                   make-withdraw---↓
;;                |                                                   |
;; ---------------|---------------------------------------------------|--------------
;;                |           ↑                                       ↓    |
;;                |           |                                     |x|x|--↑
;;                |         E1|initial-amount:100                    |
;;                |           |                                      ↓
;;                |           |                            parameters: initial-amount
;;                |           ↑                            body: ...
;;                |         E2|balance:100
;;                |           |
;;                ↓           |
;;              |x|x|---------↑
;;               |
;;               ↓
;;     parameters: amount
;;     body: (if (>= balance amount)
;;             ...
;;
;;
;; Notice, the invocation of the 'make-withdraw' body sets up two environments:
;; 1) E1 with initial-amount initialized in 100
;; 2) E2, as a result of the inner lambda (let syntactic sugar) invocation,
;;    with balance initialized in the value of initial-amount: 100
;;
;; This constructs a new procedure object, whose code is as specified by the inner lambda
;; and whose environment is E2, the environment in which the lambda was evaluated
;; to produce the procedure. The enclosing environment for E2 is E1.
;;
;; The resulting procedure object is the value returned by the call to make-withdraw.
;; This is bound to W1 in the global environment.


;; Environments created by applying the procedure object W1: (W1 50)
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env      W1---↓                                   make-withdraw---↓
;;                |                                                   |
;; ---------------|---------------------------------------------------|--------------
;;                |           ↑                                       ↓    |
;;                |           |                                     |x|x|--↑
;;                |         E1|initial-amount:100                    |
;;                |           |                                      ↓
;;                |           |                            parameters: initial-amount
;;                |           ↑                            body: ...
;;                |         E2|balance:100
;;                |           |
;;                ↓           |
;;              |x|x|---------↑
;;               |            |___________________
;;               ↓                                ↑
;;     parameters: amount                         |
;;     body: (if (>= balance amount)            E3|amount:50
;;             ...
;;
;;
;; As a result, a new frame E3 is constructed.
;; 'amount', the formal parameter of W1, is bound to the argument 50.
;; This frame has as its enclosing environment the environment E2, because
;; this is the environment that is specified by the W1 procedure object.
;;
;; Within this environment, we evaluate the body of the procedure:
;;
;; (if (>= balance amount)
;;     (begin (set! balance (- balance amount))
;;            balance)
;;     "Insufficient funds")
;;
;; The expression being evaluated references both amount and balance.
;; Amount will be found in the first frame in the environment,
;; while balance will be found by following the enclosing-environment pointer to E2.
;;
;; Notice the possibility to access and reassign 'initial-amount',
;; since it is accessible by going up the enclosing-environment path.


;; At the completions of the call to W1, balance is 50,
;; and the frame that contains balance is still pointed to
;; by the procedure object W1.
;;
;; The frame that binds amount (in which we executed the code
;; that changed balance) is no longer relevant,
;; since the procedure call that constructed it has terminated,
;; and there are no pointers to that frame from other parts
;; of the environment.
;;
;; The next time W1 is called, this will build a new frame that
;; binds amount and whose enclosing environment is E1.


;; Environments after the call to W1:
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env      W1---↓                                   make-withdraw---↓
;;                |                                                   |
;; ---------------|---------------------------------------------------|--------------
;;                |           ↑                                       ↓    |
;;                |           |                                     |x|x|--↑
;;                |         E1|initial-amount:100                    |
;;                |           |                                      ↓
;;                |           |                            parameters: initial-amount
;;                |           ↑                            body: ...
;;                |         E2|balance:50
;;                |           |
;;                ↓           |
;;              |x|x|---------↑
;;               |
;;               ↓
;;     parameters: amount
;;     body: (if (>= balance amount)
;;             ...


;; Environments produced by creating a second 'withdraw' object
;; by making another call to make-withdraw:
;;
;; (define W2 (make-withdraw 100))
;;
;; ---------------------------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env      W1---↓                                   W2---↓                                   make-withdraw---↓
;;                |                                        |                                                   |
;; ---------------|----------------------------------------|---------------------------------------------------|--------------
;;                |           ↑                            |           ↑                                       ↓    |
;;                |           |                            |           |                                     |x|x|--↑
;;                |         E1|initial-amount:100          |         E3|initial-amount:100                    |
;;                |           |                            |           |                                      ↓
;;                |           |                            |           |                            parameters: initial-amount
;;                |           ↑                            |           ↑                            body: ...
;;                |         E2|balance:50                  |         E4|balance:100
;;                |           |                            |           |
;;                ↓           |                            ↓           |
;;              |x|x|---------↑                          |x|x|---------↑
;;               |                                        |
;;               ↓                                        ↓
;;     parameters: amount                       parameters: amount
;;     body: (if (>= balance amount)            body: (if (>= balance amount)
;;             ...                                      ...
;;
;;
;; The environments E3 and E4 for W2 were created by
;; the call to make-withdraw.
;; Each of the newly created environments contains
;; a frame with its own local bindings.
;; 
;; Although W1 and W2 have the same code: the code specified by
;; the lambda expression in the body of make-withdraw, they
;; behave as independent objects.
;;
;; Calls to W1 reference the state variable balance stored in E2,
;; whereas calls to W2 reference the balance stored in E4.
;;
;; Thus, changes to the local state of one object do not affect
;; the other object.
