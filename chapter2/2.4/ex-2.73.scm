;; Exercise 2.73
;;
;; Section 2.3.2 described a program that performs symbolic differentiation:
;;
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product (multiplier exp)
;;                          (deriv (multiplicand exp) var))
;;            (make-product (deriv (multiplier exp) var)
;;                          (multiplicand exp))))
;;         <more rules can be added here>
;;         (else (error "unknown expression type -- DERIV" exp))))
;;
;; We can regard this program as performing a dispatch on the type of
;; the expression to be differentiated.
;;
;; In this situation the 'type tag' of the datum is the algebraic operator symbol
;; (such as +) and the operation being performed is deriv.
;;
;; We can transform this program into data-directed style by rewriting
;; the basic derivative procedure as:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;; a. Explain what was done above.
;;    Why can't we assimilate the predicates number? and same-variable?
;;    into the data-directed dispatch?

;; A generic version of 'deriv' was defined that is based on data-directed style.
;; Apart from two base cases, where a given expression can be a number or variable,
;; it deals with the remaining operation types by making use of
;; the operation-and-type table.
;; The table consists of only one operation: deriv, and several operator types:
;; sum, product, exponentiation, etc.
;;
;;
;; Regarding the alleged impossibility of assimilating the predicates number? and
;; same-variable? into the data-directed dispatch, one could not disagree more.
;;
;; It may not seem obvious, but after some deliberation the presence of
;; the hidden contract between the generic 'deriv' operation and all
;; the possible typed implementations of it becomes clearly visible:
;;
;;   the predicates number?, variable?, and list selectors - car and cdr -
;;   make up this hidden contract.
;;
;; No matter how strong the intention of using the 'deriv' operation as a generic one,
;; its implementation imposes some well-defined limits. Remarkably, one can see
;; these limits as a set of clauses in a contract and take advantage of them.
;;
;; Below are two possible ways of assimilating the predicates number? and
;; same-variable? into the data-directed dispatch.
;;
;; 1. The 'deriv', 'operator' and 'operands' procedures stay the same
;;
;; There is no structural distinction between a number, variable and list if
;; all these types are represented uniformly. Since it is a Lisp environment,
;; and, more importantly, 'deriv', 'operator' and 'operands' handle lists
;; as a general case, it is perfectly legal to try to represent numbers and
;; variables as general expressions (lists), where the first place is occupied by
;; the operator (for ex.: num, var) and the rest are operands.
;;
;;   Number example:   (num 5)
;;   Variable example: (var x)
;;
;; This approach requires some additional operations defined for numbers:
;; sum, multiplication and value selector. In order words, a completely
;; new data structure.
;;
;; Given the above mentioned semantics, it is only left to include
;; the corresponding procedures for derivatives of numbers and variables
;; in the 'deriv' package on a par with 'sum' and 'product'.
;;
;; As long as these procedures have been added to the operations-types table,
;; 'deriv' can be fed expressions where numbers and variable are represented
;; as lists: (num x) and (var x), respectively.
;;
;; See the 'Experimental a. 1' section below.
;;
;;
;; 2. The 'operator' and 'operands' selectors take over some of
;;    the responsibilities of the deriv procedure
;; 
;; The 'operator' and 'operands' selectors can be assigned responsible for
;; handling the base cases where a given expression can be either number or
;; variable and return the corresponding operands, whereas the general case
;; stays the same.
;;
;; In order to make it work, the operation-type table must contain the records
;; for the 'number and 'variable operands, and that is the responsibility of
;; the 'deriv' package.
;;
;; Hence, the generic 'deriv' procedure along with the 'operator' and
;; 'operands' selectors might look as follows:

(define (deriv exp var)
  ((get 'deriv (operator exp)) (operands exp) var))

(define (operator exp)
  (cond ((number? exp) 'num)
        ((variable? exp) 'var)
        (else (car exp))))

(define (operands exp)
  (if (list? exp)
      (cdr exp)
      exp))

;; The implementation of the 'deriv' package is shown in b.
;; The experimental pieces are denoted as 'Experimental a. 2'


;; b. Write the procedures for derivatives of sums and products, and
;;    the auxiliary code required to install them in the table used by
;;    the program above.

(load "../../common.scm")
(load "export-defs.scm")

(define (install-deriv-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s)
    (accumulate make-sum 0 (cdr s)))
  
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (accumulate make-product 1 (cdr p)))

  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((or (=number? m1 0) (=number? m2 0)) 0)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exp b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e))
           (expt b e))
          (else (list '** b e))))

  (define (deriv-num exp var) 0) ;; Experimental a. 2
  (define (deriv-var exp var)    ;; Experimental a. 2
    (if (same-variable? exp var) 1 0))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (multiplicand operands)
                   (deriv (multiplier operands) var))))

  (define (deriv-exp operands var)
    (make-product (exponent operands)
                  (make-product (make-exp (base operands)
                                          (make-sum (exponent operands) -1))
                                (deriv (base operands) var))))

  ;; interface to the rest of the system
  (list (export-def 'deriv 'num deriv-num) ;; Experimental a. 2
        (export-def 'deriv 'var deriv-var) ;; Experimental a. 2
        (export-def 'deriv '+ deriv-sum)
        (export-def 'deriv '* deriv-product)
        (export-def 'deriv '** deriv-exp)))

(define deriv-export-defs (install-deriv-package))


;; Tests
;;
;; (deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* (+ x 3) y))
;; (deriv '(* x y (+ x 3)) 'x)     ; (+ (* x y) (* y (+ x 3)))
;;
;; (deriv '(+ (+ x y) (* x 3)) 'x) ; 4
;; (deriv '(+ x y (* x 3)) 'x)     ; 4
;;
;; (deriv '(** x 3) 'x) ; (* 3 (** x 2))
;; (deriv '(** x y) 'x) ; (* y (** x (+ y -1)))


;; c. Choose any additional differentiation rule that you like,
;;    such as the one for exponents (ex. 2.56), and install it
;;    in this data-directed system.

;; Done as part of b.


;; d. In this simple algebraic manipulator the type of an expression
;;    is the algebraic operator that binds it together.
;;    Suppose, however, we indexed the procedures in the opposite way,
;;    so that the dispatch line in 'deriv' looked like
;;
;;    ((get (operator exp) 'deriv) (operands exp) var)
;;
;;    What corresponding changes to the derivative system are required?

;; The only change required would be the way the internal procedures
;; are interfaced to the rest of the system.
;;
;; The order of the operation and type values that identify a table entry
;; would have to be the opposite.


;; a. Experimental
;;
;; As it was mentioned above (section a),
;; there is no structural distinction between a number, variable and list if
;; all these types are represented uniformly.
;;
;; Therefore, the example (albeit quite unrealistic) below shows a way
;; to make 'num' and 'var' operations be part of data-directed programming.
;;
;; Numbers and variables are going to be treated as lists, where primitives
;; take the following form:
;; 1) number  : (num x)
;; 2) variable: (var x)

;; The initial 'deriv' procedure (along with the 'operator' and 'operands' selectors)
;; becomes utterly generic:

(define (deriv exp var)
  ((get 'deriv (operator exp)) (operands exp) var))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; A separate data structure that serves as a number representation
;;
;; A number is considered to be a list, where
;; - the first item is always 'num
;; - the second item is the value
;;
;; In addition, we define a few procedures for sum, product and expt
;; computation.

(define (make-num n)
  (list 'num n))

(define (num-value num)
  (cadr num))

(define (num? x)
  (and (pair? x) (number? (num-value x))))

(define (=num? x n)
  (and (num? x) (= (num-value x) n)))

(define (num-sum n1 n2)
  (make-num (+ (num-value n1)
               (num-value n2))))

(define (num-product n1 n2)
  (make-num (* (num-value n1)
               (num-value n2))))

(define (num-expt n1 n2)
  (make-num (expt (num-value n1)
                  (num-value n2))))

;; Next comes the deriv package that works with numbers, variables and other operations
;; in a uniform manner: it treats them all as lists.

(define (install-deriv-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s)
    (accumulate make-sum (make-num 0) (cdr s)))

  (define (multiplier p) (car p))
  (define (multiplicand p)
    (accumulate make-product (make-num 1) (cdr p)))

  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  (define (make-sum a1 a2)
    (cond ((=num? a1 0) a2)
          ((=num? a2 0) a1)
          ((and (num? a1) (num? a2))
           (num-sum a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((=num? m1 1) m2)
          ((=num? m2 1) m1)
          ((or (=num? m1 0) (=num? m2 0))
           (make-num 0))
          ((and (num? m1) (num? m2))
           (num-product m1 m2))
          (else (list '* m1 m2))))

  (define (make-exp b e)
    (cond ((=num? e 0) (make-num 1))
          ((=num? e 1) b)
          ((and (num? b) (num? e))
           (num-expt b e))
          (else (list '** b e))))
  
  (define (deriv-num operands var) (make-num 0))
  (define (deriv-var operands var)
    (if (same-variable? (car operands) var)
        (make-num 1)
        (make-num 0)))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
    (deriv (augend operands) var)))

  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (multiplicand operands)
                   (deriv (multiplier operands) var))))

  (define (deriv-exp operands var)
    (make-product (exponent operands)
                  (make-product (make-exp (base operands)
                                          (make-sum (exponent operands) (make-num -1)))
                                (deriv (base operands) var))))

  ;; interface to the rest of the system
  (list (export-def 'deriv 'num deriv-num)
        (export-def 'deriv 'var deriv-var)
        (export-def 'deriv '+ deriv-sum)
        (export-def 'deriv '* deriv-product)
        (export-def 'deriv '** deriv-exp)))

(define deriv-export-defs (install-deriv-package))


;; Tests
;;
;; (deriv '(* (* (var x) (var y)) (+ (var x) (var 3))) 'x)
;;   Value: (+ (* (var x) (var y)) (* (+ (var x) (var 3)) (var y)))
;;
;; (deriv '(* (var x) (var y) (+ (var x) (num 3))) 'x)
;;   Value: (+ (* (var x) (var y)) (* (var y) (+ (var x) (num 3))))
;;
;; (deriv '(+ (+ (var x) (var y)) (* (var x) (var 3))) 'x) ; (num 4)
;; (deriv '(+ (var x) (var y) (* (var x) (num 3))) 'x)     ; (num 4)
;;
;; (deriv '(** (var x) (num 3)) 'x) ; (* (num 3) (** (var x) (num 2)))
;; (deriv '(** (var x) (var y)) 'x) ; (* (var y) (** (var x) (+ (var y) (num -1))))
;;
;; The results are the same, but appear differently due to
;; the different representations of number and variable primitives.


;; Utils

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (get op type)
  (get-export-def op type deriv-export-defs))
