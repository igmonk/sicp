;; Exercise 3.43
;;
;; Suppose that the balances in three accounts start out as
;; $10, $20, and $30, and that multiple processes run,
;; exchanging the balances in the accounts.


;; a. Argue that if the processes are run sequentially,
;;    after any number of concurrent exchanges,
;;    the account balances should be $10, $20, and $30 in some order.

;; Each process will be blocked until after the serializer of
;; one of the participating in exchange accounts gets released.
;;
;; The work that is done by exchange is merely changing the values,
;; which, when applied in series, leads to another combination
;; of the same values.


;; b.  Draw a timing diagram like the one in figure 3.29 to show how
;;     this condition can be violated if the exchanges are implemented
;;      using the first version of the account-exchange program in this section.

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


;; The order of the read/write operations of the exchange procedure
;; has the following pattern: r r rw rw,
;; where r  stands for reading the current balance,
;;       rw stands for either serialized withdraw (read) or
;;          serialized deposit (write)
;;
;; Given 2 processes P1 and P2, each of which exchanges the acc1
;; with acc2 and acc2 with acc3, accordingly,
;; one of the possible orderings is:
;;
;; P1 (acc1, acc2): r r rw rw
;; P2 (acc2, acc3): r r       rw rw
;;
;; By the time P2 withdraws money from the acc2 bank account
;; (reading is followed by a subsequent writing),
;; P1 has already updated the value of acc2, making
;; the difference calculated by P2 invalid.
;;
;; Example:
;;
;; P1 (acc1, acc2):                 |   P2 (acc2, acc3):
;; ---------------------------------|----------------------------------
;; access acc1 = 10                 |   access acc2 = 20
;; access acc2 = 20                 |   access acc3 = 30
;; compute diff = -10               |   compute diff = -10
;; ----------------< withdraw acc1  |
;; access acc1 = 10                 |
;; set acc1 to 20                   |
;; ----------------< /              |
;; ----------------< deposit acc2   |
;; access acc2 = 20                 |
;; set acc2 to 10                   |
;; ----------------< /              |
;;                                  |   ----------------< withdraw acc2
;;                                  |   access acc2 = 10
;;                                  |   set acc2 to 20
;;                                  |   ----------------< /
;;                                  |   ----------------< withdraw acc3
;;                                  |   access acc3 = 30
;;                                  |   set acc3 to 20
;;                                  |   ----------------< /
;;                                  ↓
;;                                 time
;;
;; Hence, the result is inconsistent:
;;
;; acc1 = 20, acc2 = 20,  acc3 = 20


;; c. On the other hand, argue that even with this exchange program,
;;    the sum of the balances in the accounts will be preserved.

;; The sum of the balances in the accounts will be preserved,
;; since exactly the same value (difference) is withdrawn/deposited
;; from/to two accounts, even though this value might already be
;; invalid.


;; d. Draw a timing diagram to show how even this condition would be violated
;;    if we did not serialize the transactions on individual accounts.

;; The order of the read/write operations of the exchange procedure
;; has the following pattern: r r r w r w,
;; where r stands for reading the current balance,
;;       w stands for setting the current balance
;;
;; * 'r w' represents either not-serialized deposit or
;;         non-serialized withdraw
;;
;; Given 2 processes P1 and P2, each of which exchanges the acc1
;; with acc2 and acc2 with acc3, accordingly,
;; one of the possible orderings is:
;;
;; P1 (acc1, acc2): r r r w r   w
;; P2 (acc2, acc3): r r     r w r w
;;
;; P1 sets the balance for acc2 after P2 has modified acc2,
;; but has not modified acc3 yet (with already invalid difference).
;;
;; Example:
;;
;; P1 (acc1, acc2):                 |   P2 (acc2, acc3):
;; ---------------------------------|----------------------------------
;; access acc1 = 10                 |   access acc2 = 20
;; access acc2 = 20                 |   access acc3 = 30
;; compute diff = -10               |   compute diff = -10
;;                                  |
;; access acc1 = 10                 |
;; set acc1 to 20                   |
;;                                  |
;; access acc2 = 20                 |   access acc2 = 20
;;                                  |   set acc2 to 30
;; set acc2 to 10                   |
;;                                  |   access acc3 = 30
;;                                  |   set acc3 to 20
;;                                  ↓
;;                                 time
;;
;; Hence, the result is inconsistent and its sum is not preserved:
;;
;; acc1 = 20, acc2 = 10,  acc3 = 20
