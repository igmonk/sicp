;; Exercise 4.9
;;
;; Many languages support a variety of iteration constructs,
;; such as do, for, while, and until.
;;
;; In Scheme, iterative processes can be expressed in terms of
;; ordinary procedure calls, so special iteration constructs
;; provide no essential gain in computational power.
;;
;; On the other hand, such constructs are often convenient.
;;
;; Design some iteration constructs, give examples of their use,
;; and show how to implement them as derived expressions.


;; The iteration construct 'while' has been designed.
;;
;; Start with sketching the syntax of the form 'while':
;;
;;   (while <predicate> <body>)
;;
;; Repeatedly executes body while test <predicate> is true.
;; Presumes some side-effect will cause <predicate> to become false.


;; An example of the form's usage in computing factorial:

(define (factorial x)
  (let ((n 1)
        (i x))
    (while (> i 0)
           (begin
             (set! n (* n i))
             (set! i (- i 1))))
    n))

;; As can be seen, 'while' might expect some variables to be
;; defined, and presumes some side-effect will cause <predicate>
;; to become false.
;;
;; This construction is a loop and can be modelled with
;; the help of named 'let'.


;; The first approach to model 'while' can take form of
;; a named 'let' parameterized with some state:

(define (factorial x)
  (let loop ((n 1)
             (i x))
    (if (> i 0)
        (loop (* n i) (- i 1))
        n)))

;; (factorial 1) ; 1
;; (factorial 2) ; 2
;; (factorial 3) ; 6
;; (factorial 4) ; 24
;; (factorial 5) ; 120


;; In order to avoid the complexity of working with
;; the named 'let' parameters/arguments and their order,
;; the state can be enclosed in an outer unnamed 'let'.
;;
;; That allows us to implement a loop construction that
;; does not require any parameters - an inner named 'let': 

(define (factorial x)
  (let ((n 1)
        (i x))
    (let loop ()
      (if (> i 0)
          (begin
            (set! n (* n i))
            (set! i (- i 1))
            (loop))))
    n))

;; (factorial 1) ; 1
;; (factorial 2) ; 2
;; (factorial 3) ; 6
;; (factorial 4) ; 24
;; (factorial 5) ; 120

;; This inner named 'let' becomes the target result of
;; the derived expression 'while'.


;; See: eval-while.scm
;;      evaluator-tests.scm
