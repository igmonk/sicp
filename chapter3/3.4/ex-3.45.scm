;; Exercise 3.45
;;
;; Louis Reasoner thinks our bank-account system is unnecessarily complex
;; and error-prone now that deposits and withdrawals aren't automatically
;; serialized.
;; He suggests that make-account-and-serializer should have exported
;; the serializer (for use by such procedures as serialized-exchange)
;; in addition to (rather than instead of) using it to serialize accounts
;; and deposits as make-account did.
;;
;; He proposes to redefine accounts as follows:

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; Then deposits are handled as with the original make-account:

(define (deposit account amount)
  ((account 'deposit) amount))

;; Explain what is wrong with Louis's reasoning.
;; In particular, consider what happens when serialized-exchange is called.


;; The solution proposed by Lois Reasoner lead to a deadlock.
;;
;; Whenever a serialized-exchange is called, it acquires the accounts'
;; locks (serializers) and calls the exchange procedure, which, in turn,
;; tries to acquire exactly the same locks (serializers) by calling
;; 'withdraw or 'deposit methods. Since, only one execution of a procedure
;; in each serialized set is permitted to happen at a time,
;; the proccess gets blocked and is stalled forever.
