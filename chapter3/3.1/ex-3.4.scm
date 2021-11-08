;; Exercise 3.4
;;
;; Modify the make-account procedure of exercise 3.3 by adding
;; another local state variable so that, if an account is accessed
;; more than seven consecutive times with an incorrect password,
;; it invokes the procedure call-the-cops.


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    "Calling the cops... Run!")
  (let ((incorrect-pwd-counter 0)
        (max-attempts 7))
    (define (dispatch pwd m)
      (if (<= incorrect-pwd-counter max-attempts)
          (if (eq? pwd password)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request -- MAKE-ACCOUNT"
                                 m)))
              (lambda (_)
                (set! incorrect-pwd-counter (+ incorrect-pwd-counter 1))
                (if (> incorrect-pwd-counter max-attempts)
                    (call-the-cops)
                    "Incorrect password")))
          (lambda (_) "The account is blocked. The cops are on the way. Run!")))
    dispatch))


;; (define acc (make-account 100 'secret-password))

;; ((acc 'secret-password 'withdraw) 40) ; 60
;; ((acc 'secret-password 'deposit) 50)  ; 110

;; ((acc 'wrong-password 'withdraw) 1) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 2) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 3) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 4) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 5) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 6) ; "Incorrect password"
;; ((acc 'wrong-password 'withdraw) 7) ; "Incorrect password"

;; ((acc 'wrong-password 'withdraw) 8) ; "Calling the cops... Run!"

;; ((acc 'wrong-password 'withdraw) 8) ; "The account is blocked. The cops are on the way. Run!"
;; ((acc 'wrong-password 'withdraw) 8) ; "The account is blocked. The cops are on the way. Run!"
;; ((acc 'wrong-password 'withdraw) 8) ; "The account is blocked. The cops are on the way. Run!"
