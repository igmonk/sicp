;; Exercise 4.34
;;
;; Modify the driver loop for the evaluator so that lazy pairs and lists
;; will print in some reasonable way.
;;
;; (What are you going to do about infinite lists?)
;;
;; You may also need to modify the representation of lazy pairs so that
;; the evaluator can identify them in order to print them.


;; Start with renaming of the primitive procedures cons, car and cdr
;; to _cons, _car and _cdr, respectively:

(define primitive-procedures
  (list (list '_car car)
        (list '_cdr cdr)
        (list '_cons cons)
        <...>
        ))


;; Define the compound procedured cons, car and cdr that will work
;; with a structure which car will be 'lazy-list:

(eval-exp '(define (cons x y)
             (_cons 'lazy-list (lambda (m) (m x y)))))

(eval-exp '(define (car z) ((_cdr z) (lambda (p q) p))))
(eval-exp '(define (cdr z) ((_cdr z) (lambda (p q) q))))


;; Next, define the lazy-list? predicate and extend the procedure
;; user-print (used by the driver loop) with the case for lazy lists:

(define (lazy-list? object)
  (tagged-list? object 'lazy-list))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-list? object)
         (print-lazy-list object))
        (else
         (display object))))


;; The procedure print-lazy-list is responsible for printing the elements
;; of the given list of undefined length inside the curly braces.
;;
;; The procedure generates an iterative process that enumerates the list
;; of elements until it reaches an unevaluated thunk and prints
;; - the value of each evaluated thunk, and
;; - '...' for the rest

(define (print-lazy-list lazy-list)
  (define (inner ll)
    (when (not (null? ll))
      (let ((first (lazy-list-car-value ll)))
        (if (evaluated-thunk? first)
            (let ((rest (lazy-list-cdr-value ll)))
              (display (thunk-value first))
              (display " ")
              (if (evaluated-thunk? rest)
                  (let ((ll-rest (thunk-value rest)))
                    (if (implicit-infinite-list? ll ll-rest)
                        (display "...")
                        (inner ll-rest)))
                  (display "...")))
            (display "...")))))
  (display "{")
  (inner lazy-list)
  (display "}"))


;; The way the inner procedure access the elements of the given list
;; to fetch its thunks is based on looking up the variables x and y
;; corresponding to the car and cdr, of the lazy list, respectively,
;; in the current thunk's environment.
;;
;; Based on the definition of cons:
;;
;; (define (cons x y)
;;   (_cons 'lazy-list (lambda (m) (m x y)))))
;;
;; The environment of the lazy list ll can be fetched by
;;
;; (procedure-environment (cdr ll))
;;
;; Hence, the following procedures are made use of:

(define (lazy-list-car-value ll)
  (lookup-variable-value 'x (procedure-environment (cdr ll))))

(define (lazy-list-cdr-value ll)
  (lookup-variable-value 'y (procedure-environment (cdr ll))))

;; Notice, their equivalents could be:
;;
;; (eval (car pair)) and (eval (cdr pair))


;; Now it only remains to implement a way of detecting infinite lists
;; defined implicitly (without generating procedures), such as:
;;
;; (define ones (cons 1 ones))
;;
;; After the second element of this list has been forced,
;; such a list won't be containing any unevaluated thunks,
;; which will inevitably lead to the infinite printing.
;;
;; A possible solution to this could be a procedure that
;; identifies such lists by comparing the environments of
;; two successive elements (evaluated thunks) as described below.


;; The procedure identifies whether the given stream is
;; an implicitly defined one (without generating procedure), for ex.:
;;
;; (define ones (cons 1 ones)) ; * normal-order evaluation
;;
;; For that, we need to compare the environments of the procedures
;; accessing the current and following items of the given lazy list.
;;
;; The procedure is useful to avoid the infinite printing loop,
;; since as soon as the first two elements of an implicitly defined
;; lazy list have been forced (= transformed to evaluated thunks),
;; a simple check for evaluated-thunk? will not suffice for the
;; following reason:
;;
;; 1. car of the lazy-list will refer to an evaluated thunk that
;;        keeps the actual value (1 in case of ones)
;; 2. cdr of the lazy-list will refer to an evaluated thunk that
;;        points to the lazy list, whose car is described in 1.,
;;        and whose cdr points to the evaluated thunk described in 2.,
;;        leading to the infinite recursion
;;
;; Due to the inner structure of lazy lists, this infinite recursion
;; can be detected by comparing the environments of the procedures
;; representing the successive items of the list.
(define (implicit-infinite-list? ll ll-rest)
  (eq? (procedure-environment (cdr ll))
       (procedure-environment (cdr ll-rest))))


;; See: evaluator-test.scm
;;      test-utils.scm
