;; Exercise 3.36
;;
;; Suppose we evaluate the following sequence of expressions
;; in the global environment:
;;
;; (define a (make-connector))
;; (define b (make-connector))
;; (set-value! a 10 'user)
;;
;; At some time during evaluation of the set-value!,
;; the following expression from the connector's local procedure
;; is evaluated:
;;
;; (for-each-except setter inform-about-value constraints)
;;
;; Draw an environment diagram showing the environment in which
;; the above expression is evaluated.


;; Result of evaluating
;;
;; (define a (make-connector))
;; (define b (make-connector))
;;
;; ------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env           a---↓                                b---↓                          set-value!---↓
;;                    |                                    |                                       |
;; -------------------|----------------------------------------------------------------------------------
;;                    |    ↑                               |    ↑                                  |    ↑
;;                    |    |                               |    |                                  |    |
;;                    |  E1|value:false                    |  E2|value:false                     |x|x|--↑
;;                    |    |informant:false                |    |informant:false                  |
;;                    |    |constraints:'()                |    |constraints:'()                  ↓
;;                    |    |                               |    |                       parameters:connector
;;                    ↓    |                               ↓    |                                  new-value
;;                  |x|x|--↑                             |x|x|--↑                                  informant
;;                   |                                    |                                   body:((connector 'set-value!) ...)
;;                   ↓                                    ↓
;;         parameters: request                  parameters: request
;;         body: (define set-my-value ...)      body: (define set-my-value ...)
;;               (define forget-my-value ...)         (define forget-my-value ...)
;;               (define connect ...)                 (define connect ...)
;;               me                                   me



;; Evaluation of (set-value! a 10 'user) creates a new environment - E3 - in which
;; the body of set-value! is to be evaluated:
;;
;; ------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env      set-value!---↓                                                          a---↓   b---↓
;;                        |                                                              |       |
;; -----------------------|--------------------------------------------------------------.-------.-------
;;                        |    ↑                              ↑                          .       .
;;                        |    |                              |
;;                      |x|x|--↑                            E3|connector:a
;;                       |                                    |new-value:10
;;                       ↓                                    |informant:'user
;;             parameters:connector
;;                        new-value                           ((connector 'set-value!) new-value informant)
;;                        informant
;;                   body:((connector 'set-value!) ...)


;; Evaluation of (connector 'set-value!)
;;
;; creates a new environment - E4 - in which the call to
;; the inner procedure set-my-value is to be evaluated:
;;
;; ------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env                                                            a---↓                     b---↓
;;                                                                     |                         |
;; --------------------------------------------------------------------|-------------------------.-------
;;               ↑                                                     |    ↑                    .
;;               |                                                     |    |
;;             E3|connector:a                                          |  E1|value:false
;;               |new-value:10                                         |    |informant:false
;;               |informant:'user                                      |    |constraints:'()
;;               |                                                     |    |_______________
;;               ((connector 'set-value!) new-value informant)         ↓    |              ↑
;;                                                                   |x|x|--↑              |
;;                                                                    |                  E4|request:'set-value!
;;                                                                    ↓
;;                                                          parameters: request            <call to set-my-value>
;;                                                                body: <body>


;; Evaluation of (set-my-value new-value informant)
;;
;; creates a new environment: E5
;;
;; ------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env                                                            a---↓                     b---↓
;;                                                                     |                         |
;; --------------------------------------------------------------------|-------------------------.-------
;;               ↑                                                     |    ↑                    .
;;               |                                                     |    |
;;             E3|connector:a                                          |  E1|value:false
;;               |new-value:10                                         |    |informant:false
;;               |informant:'user                                      |    |constraints:'()
;;               |                                                     |    |_______________
;;               ((connector 'set-value!) new-value informant)         ↓    |        ↑
;;                                                                   |x|x|--↑        |
;;                                                                    |              |
;;                                                                    ↓              |
;;                                                          parameters: request      |
;;                                                                body: <body>       |
;;                                                                                   |
;;                                                                                   |
;;                                                                                 E5|newval:10
;;                                                                                   |informant:'user
;;
;;                                                                                   (cond ((not (has-value? me)) ...))


;; At the brink of the evaluation of
;;
;; (for-each-except setter inform-about-value constraints)
;;
;; the inner state of 'a' will have received new values for
;; its value and informant components:
;;
;; ------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env                                                            a---↓                b---↓
;;                                                                     |                    |
;; --------------------------------------------------------------------|--------------------.------------
;;               ↑                                                     |    ↑               .
;;               |                                                     |    |
;;             E3|connector:a                                          |  E1|value:10
;;               |new-value:10                                         |    |informant:'user
;;               |informant:'user                                      |    |constraints:'()
;;               |                                                     |    |_______________
;;               ((connector 'set-value!) new-value informant)         ↓    |        ↑
;;                                                                   |x|x|--↑        |
;;                                                                    |              |
;;                                                                    ↓              |
;;                                                          parameters: request      |
;;                                                                body: <body>       |
;;                                                                                   |
;;                                                                                   |
;;                                                                                 E5|newval:10
;;                                                                                   |informant:'user
;;
;;                                                                                   (cond ((not (has-value? me)) ...))
;;
;; The inner state of 'b' stays untouched.
