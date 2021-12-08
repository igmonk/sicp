;; Exercise 3.44
;;
;; Consider the problem of transferring an amount
;; from one account to another.
;;
;; Ben Bitdiddle claims that this can be accomplished with
;; the following procedure, even if there are multiple people
;; concurrently transferring money among multiple accounts,
;; using any account mechanism that serializes deposit and
;; withdrawal transactions,
;; for example, the version of make-account in the text above.

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;; Louis Reasoner claims that there is a problem here,
;; and that we need to use a more sophisticated method,
;; such as the one required for dealing with the exchange problem.
;;
;; Is Louis right?
;; If not, what is the essential difference between the transfer problem
;; and the exchange problem?
;; (You should assume that the balance in 'from-account' is at least amount.)


;; Louis Reasoner's claim is not correct here.
;;
;; The essential difference between the transfer problem and
;; the exchange problem lies at the level of consistency guarantees.
;;
;; 1. exchange => fully atomic
;;
;; The exchange problem requires a solution that guarantees
;; the invariability of the total amount in the accounts before
;; and after the exchange. That implies the atomicity of the exhange
;; transaction, which can be solved by serializing the whole operation.
;;
;; 2. transfer => per-account atomicity, eventual consistency
;;
;; The transfer problem does not impose any requirements on the pair
;; of accounts involved in the operation, only on the deposit and
;; withdrawal operations, that are serialized by default.
;; That leads to eventual consistency which is sufficient.
