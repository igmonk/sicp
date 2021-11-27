;; Exercise 3.27
;;
;; Memoization (also called tabulation) is a technique that
;; enables a procedure to record, in a local table,
;; values that have previously been computed.
;;
;; This technique can make a vast difference in the performance of a program.
;;
;; A memoized procedure maintains a table in which values of
;; previous calls are stored using as keys the arguments
;; that produced the values.
;;
;; When the memoized procedure is asked to compute a value,
;; it first checks the table to see if the value is already there and,
;; if so, just returns that value.
;; Otherwise, it computes the new value in the ordinary way and
;; stores this in the table.
;;
;; As an example of memoization, recall from section 1.2.2
;; the exponential process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; (fib 7) ; 13
;; (fib 8) ; 21
;; (fib 9) ; 34

;; The memoized version of the same procedure is

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; where the memoizer is defined as

(load "table-md-bst.scm")

(define int-comp (make-comparator integer? = < number-hash))

(define (memoize f)
  (let ((table (make-table int-comp)))
    (let ((get (table 'lookup))
          (put (table 'insert!)))
      (lambda (x . args)
        (let ((value (get (cons x args))))
          (or value
              (let ((result (apply f (cons x args))))
                (put (cons x args) result)
                result)))))))

;; Draw an environment diagram to analyze the computation of (memo-fib 3).
;;
;; Explain why memo-fib computes the n-th Fibonacci number
;; in a number of steps proportional to n.
;;
;; Would the scheme still work if we had simply defined memo-fib
;; to be (memoize fib)?

;; (memo-fib 7) ; 13
;; (memo-fib 8) ; 21
;; (memo-fib 9) ; 34

;; (memo-fib 30) ; 832040 (completes almost instantly)
;; (fib 30)      ; 832040 (takes a few seconds to complete)


;; memo-fib computes the n-th Fibonacci number in a number of steps
;; proportional to n because it computes the same result once.
;;
;; The scheme would not work had we simply defined memo-fib to be
;; (memoize fib), since inside fib there is a recursive call
;; to itself instead of the memoized procedure that performs
;; table lookup.
