;; Exercise 3.41
;;
;; Ben Bitdiddle worries that it would be better
;; to implement the bank account as follows
;; (where the commented line has been changed):

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ; serialized
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; because allowing unserialized access to the bank balance
;; can result in anomalous behavior.
;;
;; Do you agree?
;; Is there any scenario that demonstrates Ben's concern?


;; Unserialized access does not lead to anomalous behaviour,
;; since both withdraw and deposit procedures make a single
;; modification to the shared variable. Hence, at each point
;; in time a reader sees the balance value either before or
;; after a certain operation has performed completely.
;;
;; There might be a case when a single operation would require
;; multiple modifications of the shared state, so that
;; unserialized access to this state would result in
;; some intermidiate values.
;; It depends on concrete requirements,
;; whether such a behaviour is undesirable or not.
