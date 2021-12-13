;; 3.4 Concurrency: Time Is of the Essence
;;
;; The difficulty in dealing with concurrent processes is rooted
;; in the need to consider the interleaving of the order of events
;; in the different processes.
;;
;; As programmers designing a system, we would have to consider
;; the effects of each of the possible orderings and check that
;; each behaviour is acceptable.
;;
;; Such an approach rapidly becomes unwieldy as the numbers of
;; processes and events increase.


;; A more practical approach to the design of concurrent systems is
;; to devise general mechanisms that allow us to constrain
;; the interleaving of concurrent processes so that we can be sure
;; that the program behavior is correct.
;;
;; Many mechanisms have been developed for this purpose.
;;
;; Below is one of the described, the 'serializer'.


;; Suppose that we have extended Scheme to include a procedure:
;;
;; (parallel-execute <p1> <p2> ... <pk>)
;;
;; Each <p> must be a procedure of no arguments.
;;
;; parallel-execute creates a separate process for each <p>,
;; which applies <p> (to no arguments).
;;
;; These processes all run concurrently.


;; An example of how this is used:
;;
;; (define x 10)
;;
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (+ x 1))))
;;
;; This creates two concurrent processes:
;; 1. P1, which sets x to x times x
;; 2. P2, which increments x
;;
;; After execution is complete, x will be left with
;; one of five possible values, depending on the interleaving
;; of the events of P1 and P2:
;;
;; 101: P1 sets x to 100 and then P2 increments x to 101
;; 121: P2 increments x to 11 and then P1 sets x to x times x
;; 110: P2 changes x from 10 to 11 between the two times that
;;      P1 accesses the value of x during the evaluation of (* x x)
;; 11:  P2 accesses x, then P1 sets x to 100, then P2 sets x
;; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x


;; We can constrain the concurrency by using serialized procedures,
;; which are created by serializers.
;; Serializers are constructed by make-serializer, whose implementation
;; is given below.
;;
;; A serializer takes a procedure as argument and returns
;; a serialized procedure that behaves like the original procedure.
;; All calls to a given serializer return serialized procedures
;; in the same set.


;; Thus, in contrast to the example above, executing
;;
;; (define x 10)
;; (define s (make-serializer))
;;
;; (parallel-execute (s (lambda () (set! x (* x x))))
;;                   (s (lambda () (set! x (+ x 1)))))
;;
;; can produce only two possible values for x, 101 or 121.
;;
;; The other possibilities are eliminated, because
;; the execution of P1 and P2 cannot be interleaved.


;; Complexity of using multiple shared resources
;;
;; While using serializers is relatively straightforward when
;; there is only a single shared resource (such as a single bank account),
;; concurrent programming can be treacherously difficult when
;; there are multiple shared resources.

;; Suppose we wish to swap the balances in two bank accounts.
;;
;; We access each account to find the balance,
;; compute the difference between the balances,
;; withdraw this difference from one account,
;; and deposit it in the other account.
;;
;; We could implement this as follows:

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; This procedure works well when only a single process
;; is trying to do the exchange.
;;
;; For correct behavior in a concurrent environment,
;; we must arrange for the exchange procedure
;; to lock out any other concurrent accesses to the accounts
;; during the entire time of the exchange.
;;
;; One way we can accomplish this is by using both accounts' serializers
;; to serialize the entire exchange procedure.

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
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; We can use this to do serialized deposits and withdrawals.
;;
;; However, unlike our earlier serialized account,
;; it is now the responsibility of each user of bank-account objects
;; to explicitly manage the serialization, for example as follows:
;;
;; (see ex-3.45)

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

;; Exporting the serializer in this way gives us enough flexibility
;; to implement a serialized exchange program.
;;
;; We simply serialize the original exchange procedure with
;; the serializers for both accounts:

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;; Implementing serializers
;;
;; Each serializer has an associated mutex.
;; Given a procedure p, the serializer returns a procedure that
;; acquires the mutex, runs `p`, and then releases the mutex.
;;
;; This ensures that only one of the procedures produced by the serializer
;; can be running at once, which is precisely the serialization property
;; that we need to guarantee.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; The mutex is a mutable object
;; (here a one-element list is used, which is referred to as a cell)
;; that can hold the value true or false.
;;
;; When the value is false, the mutex is available to be acquired.
;; When the value is true, the mutex is unavailable,
;; and any process that attempts to acquire the mutex must wait.

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

;; test-and-set! tests the cell and returns the result of the test.
;; In addition, if the test was false, test-and-set! sets the cell contents
;; to true before returning false.
;;
;; This behaviour can be expressed as the following procedure:

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; However, this implementation of test-and-set! does not suffice as it stands.
;;
;; There is a crucial subtlety here, which is the essential place where
;; concurrency control enters the system: The test-and-set! operation
;; must be performed atomically.
;;
;; That is, we must guarantee that, once a process has tested the cell
;; and found it to be false, the cell contents will actually be set to
;; true before any other process can test the cell.

;; In MIT Scheme for a single processor, which uses a time-slicing model,
;; test-and-set! can be implemented as follows:

(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

;; without-interrupts disables time-slicing interrupts
;; while its procedure argument is being executed.
;;
;; Alternatively, multiprocessing computers provide instructions
;; that support atomic operations directly in hardware.


;; Deadlock
;;
;; Account exchanging still has a problem, even with
;; the serialized-exchange procedure above.
;;
;; When each process is stalled forever, waiting for the other,
;; the situation is called a 'deadlock'.
;;
;; Deadlock is always a danger in systems that provide concurrent access
;; to multiple shared resources.
;;
;; One way to avoid the deadlock in this situation is to give each account
;; a unique identification number and rewrite serialized-exchange so that
;; a process will always attempt to enter a procedure protecting
;; the lowest-numbered account first.
;;
;; Although this method works well for the exchange problem,
;; there are other situations that require more sophisticated
;; deadlock-avoidance techniques, or where deadlock cannot be avoided at all.
