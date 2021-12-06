;; Exercise 3.38
;;
;; Suppose that Peter, Paul, and Mary share a joint bank account
;; that initially contains $100.
;;
;; Concurrently, Peter deposits $10, Paul withdraws $20,
;; and Mary withdraws half the money in the account,
;; by executing the following commands:
;;
;; Peter: (set! balance (+ balance 10))
;; Paul:  (set! balance (- balance 20))
;; Mary:  (set! balance (- balance (/ balance 2)))
;;
;; a. List all the different possible values for balance
;;    after these three transactions have been completed,
;;    assuming that the banking system forces the three processes
;;    to run sequentially in some order.
;;
;; b. What are some other values that could be produced if
;;    the system allows the processes to be interleaved?
;;    Draw timing diagrams like the one in figure 3.29
;;    to explain how these values can occur.


;; a. All the different possible values for balance
;;
;; 1. Peter -> Paul -> Mary => balance = $45
;; 2. Peter -> Mary -> Paul => balance = $35
;; 3. Paul -> Peter -> Mary => balance = $45
;; 4. Paul -> Mary -> Peter => balance = $50
;; 5. Mary -> Peter -> Paul => balance = $40
;; 6. Mary -> Paul -> Peter => balance = $40


;; b. Some other values that could be produced if the system
;;    allows the processes to be interleaved
;;
;; 1. Consider the case when all the participants read the same
;;    balance value based on which they deduce what is to be set
;;    by the end of their transaction:
;;
;;    Peter: read 100
;;    Paul:  read 100
;;    Mary:  read 100
;;    Peter: write (+ 100 10) => balance = $110
;;    Paul:  write (- 100 20) => balance = $80
;;    Mary:  write (/ 100 2)  => balance = $50
;;
;;    Alhought, the result corresponds to one of the possible
;;    orders (4, Paul -> Mary -> Peter), it can only be considered
;;    as a coincidence.
;;
;; 2. Another example that results in an inconsistent state:
;;
;;    Peter: read 100
;;    Paul:  read 100
;;    Paul:  write (- 100 20) => balance = $80
;;    Peter: write (+ 100 10) => balance = $110
;;    Mary:  read 110
;;    Mary:  write (/ 110 2)  => balance = $55
;;
;;    The result does not correspond to any of the possible orders
;;    and exemplifies the case when the total amount of money
;;    in the system is not conserved.
