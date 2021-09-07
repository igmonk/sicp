;; Exercise 2.20
;;
;; The procedures +, *, and list take arbitrary numbers of arguments.
;;
;; One way to define such procedures is to use 'define' with 'dotted-tail notation'.
;;
;; In a procedure definition, a parameter list that has a dot before the last parameter name
;; indicates that, when the procedure is called, the initial parameters (if any)
;; will have as values the initial arguments, as usual, but the final parameter's value
;; will be a list of any remaining arguments.
;;
;; For instance, given the definition
;;
;; (define (f x y . z) <body>)
;;
;; the procedure f can be called with two or more arguments. If we evaluate
;;
;; (f 1 2 3 4 5 6)
;;
;; then in the body of f:
;; - x will be 1
;; - y will be 2
;; - z will be the list (3 4 5 6)
;;
;; Given the definition
;;
;; (define (g . w) <body>)
;;
;; the procedure g can be called with zero or more arguments. If we evaluate
;;
;; (g 1 2 3 4 5 6)
;;
;; then in the body of g:
;; - w will be the list (1 2 3 4 5 6)
;;
;; Use this notation to write a procedure 'same-parity' that takes one or more integers
;; and returns a list of all the arguments that have the same even-odd parity as the first argument.
;;
;; For example,
;;
;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)
;;
;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)


;; Start with a predicate that tests the parity of a given numer.

(define (parity-predicate n)
  (if (odd? n) odd? even?))


;; Define a function that is responsible for filtering a given list
;; based on a particular predicate.
;;
;; (let's pretend we know nothing about the built-in procedure 'filter')
;;
;; The following procedure has an inner definition that generates branches of both process types:
;; - iterative (when the current item has to be skipped)
;; - recursive (when the current item has to be added to the resulting list)
;;
;; The reason behind the need to have an inner process of the recursive type
;; is the reluctance to use 'append', which by its nature is recursive anyway.
;;
;; An alternative solution to this case (using append) would be:
;;
;; ((predicate (car l))
;;  (inner (cdr l) (append result (list (car l)))))

(define (filter-list predicate items)
  (define (inner l result)
    (cond ((null? l)
           result)
          ((predicate (car l))
           (cons (car l) (inner (cdr l) result)))
          (else
           (inner (cdr l) result))))
  (inner items '()))

;; (filter-list odd? (list 1 2 3 4 5 6 7))  ; (1 3 5 7)
;; (filter-list even? (list 1 2 3 4 5 6 7)) ; (2 4 6)


;; Finally, define the procedure aimed to find the elements of the same parity,
;; that makes use of the procedure defined above:

(define (same-parity n . rest)
  (filter-list (parity-predicate n)
               (cons n rest)))

;; (same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
;; (same-parity 2 3 4 5 6 7)   ; (2 4 6)
