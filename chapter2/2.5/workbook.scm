;; 2.5 Systems with Generic Operations

;; From the perspective of someone using numbers,
;; there is a single procedure 'add' that operates on whatever numbers are supplied.
;;
;; 'add' is part of a generic interface that allows the separate ordinary-arithmetic,
;; rational-arithmetic, and complex-arithmetic packages to be accessed uniformly
;; by programs that use numbers.
;;
;; Any individual arithmetic package (such as the complex package) may itself be accessed
;; through generic procedures (such as 'add-complex') that combine packages
;; designed for different representations (such as rectangular and polar).
;;
;; Moreover, the structure of the system is additive,
;; so that one can design the individual arithmetic packages separately and
;; combine them to produce a generic arithmetic system.


;;                           Programs that use numbers
;;
;; -------------------------------| add sub mul div |--------------------------
;;
;;                             Generic arithmetic package
;;
;;    | add-rat sub-rat |     | add-complex sub-complex |     |            |
;; ---|                 |-----|                         |-----|   + - * /  |---
;;    | mul-rat div-rat |  |  | mul-complex div-complex |  |  |            |
;;                         |                               |
;;                         |                               |
;;         Rational        |       Complex arithmetic      |     Ordinary
;;        Arithmetic       |-------------------------------|    Arithmetic
;;                         |   Rectangular |     Polar     |
;;-----------------------------------------------------------------------------

;; Notice the abstraction barriers.
;; From the perspective of someone using numbers,
;; there is a single procedure 'add' that operates on whatever numbers are supplied.
;; 'add' is part of a generic interface that allows the separate ordinary-arithmetic,
;; rational-arithmetic, and complex-arithmetic packages to be accessed uniformly
;; by programs that use numbers.

(load "../../common.scm")
(load "../export-defs.scm")


;; Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


;; We begin by installing a package for handling 'ordinary' numbers, that is,
;; the primitive numbers of our language.
;; We will tag these with the symbol 'scheme-number'.
;; The arithmetic operations in this package are the primitive arithmetic procedures
;; (so there is no need to define extra procedures to handle the untagged numbers).
;;
;; Since these operations each take two arguments,
;; they are installed in the table keyed by the list '(scheme-number scheme-number):

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (list (export-def 'add '(scheme-number scheme-number)
                    (lambda (x y) (tag (+ x y))))
        (export-def 'sub '(scheme-number scheme-number)
                    (lambda (x y) (tag (- x y))))
        (export-def 'mul '(scheme-number scheme-number)
                    (lambda (x y) (tag (* x y))))
        (export-def 'div '(scheme-number scheme-number)
                    (lambda (x y) (tag (/ x y))))
        (export-def 'make 'scheme-number
                    (lambda (x) (tag x)))))

(define scheme-number-package-exports (install-scheme-number-package))

;; Users of the scheme-number package will create (tagged) ordinary numbers
;; by means of the procedure:

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; Now that the framework of the generic arithmetic system is in place,
;; we can readily include new kinds of numbers.


;; Rational arithmetic package

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (list (export-def 'add '(rational rational)
                    (lambda (x y) (tag (add-rat x y))))
        (export-def 'sub '(rational rational)
                    (lambda (x y) (tag (sub-rat x y))))
        (export-def 'mul '(rational rational)
                    (lambda (x y) (tag (mul-rat x y))))
        (export-def 'div '(rational rational)
                    (lambda (x y) (tag (div-rat x y))))
        (export-def 'make 'rational
                    (lambda (n d) (tag (make-rat n d))))))

(define rational-package-exports (install-rational-package))

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; Rectangular complex numbers package (section 2.4.1)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (list (export-def 'real-part '(rectangular) real-part)
        (export-def 'imag-part '(rectangular) imag-part)
        (export-def 'magnitude '(rectangular) magnitude)
        (export-def 'angle '(rectangular) angle)
        (export-def 'make-from-real-imag 'rectangular
                    (lambda (x y) (tag (make-from-real-imag x y))))
        (export-def 'make-from-mag-ang 'rectangular
                    (lambda (r a) (tag (make-from-mag-ang r a))))))

(define rectangular-package-exports (install-rectangular-package))


;; Polar complex numbers package (section 2.4.1)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (list (export-def 'magnitude '(polar) magnitude)
        (export-def 'angle '(polar) angle)
        (export-def 'real-part '(polar) real-part)
        (export-def 'imag-part '(polar) imag-part)
        (export-def 'make-from-mag-ang 'polar
                    (lambda (x y) (tag (make-from-mag-ang x y))))
        (export-def 'make-from-real-imag 'polar
                    (lambda (x y) (tag (make-from-real-imag x y))))))

(define polar-package-exports (install-polar-package))


;; Complex arithmetic package
;;
;; Additivity permits us to use, as the internal operations,
;; the same 'add-complex', 'sub-complex', 'mul-complex', and 'div-complex'
;; procedures from section 2.4.1.

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (list (export-def 'add '(complex complex)
                    (lambda (z1 z2) (tag (add-complex z1 z2))))
        (export-def 'sub '(complex complex)
                    (lambda (z1 z2) (tag (sub-complex z1 z2))))
        (export-def 'mul '(complex complex)
                    (lambda (z1 z2) (tag (mul-complex z1 z2))))
        (export-def 'div '(complex complex)
                    (lambda (z1 z2) (tag (div-complex z1 z2))))
        (export-def 'make-from-real-imag 'complex
                    (lambda (x y) (tag (make-from-real-imag x y))))
        (export-def 'make-from-mag-ang 'complex
                    (lambda (r a) (tag (make-from-mag-ang r a))))))

(define complex-package-exports (install-complex-package))

;; Programs outside the complex-number package can construct complex numbers
;; either from real and imaginary parts or from magnitudes and angles.
;;
;; Notice how the underlying procedures, originally defined in
;; the rectangular and polar packages, are exported to the complex package,
;; and exported from there to the outside world.


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; What we have here is a two-level tag system.
;; A typical complex number, such as 3 + 4i in rectangular form,
;; would be represented as:
;; 1. The outer tag (complex) is used to direct the number to the complex package.
;; 2. Once within the complex package, the next tag (rectangular) is used to
;;    direct the number to the rectangular package.
;;
;; In a large and complicated system there might be many levels,
;; each interfaced with the next by means of generic operations.
;;
;; As a data object is passed 'downward', the outer tag that is used to direct it
;; to the appropriate package is stripped off (by applying contents) and
;; the next level of tag (if any) becomes visible to be used for further dispatching.

;; Once the procedure definitions are internal, however,
;; they no longer need names that are distinct from each other:
;; we could simply name them 'add', 'sub', 'mul', and 'div' in these packages.

(define (get op type)
   (get-export-def op
                   type
                   (append scheme-number-package-exports
                           rational-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports)))


;; Tests 1: Scheme numbers
;;
;; (define n1 (make-scheme-number 10))
;; (define n2 (make-scheme-number 20))
;;
;; (define n1+n2 (add n1 n2))
;; (define n1-n2 (sub n1 n2))
;; (define n1*n2 (mul n1 n2))
;; (define n1/n2 (div n1 n2))
;;
;; (type-tag n1+n2) ; scheme-number
;; (contents n1+n2) ; 30
;;
;; (type-tag n1-n2) ; scheme-number
;; (contents n1-n2) ; -10
;;
;; (type-tag n1*n2) ; scheme-number
;; (contents n1*n2) ; 200
;;
;; (type-tag n1/n2) ; scheme-number
;; (contents n1/n2) ; 1/2


;; Tests 2: Rational numbers
;;
;; (define r1 (make-rational 1 2))
;; (define r2 (make-rational 4 5))
;;
;; (define r1+r2 (add r1 r2))
;; (define r1-r2 (sub r1 r2))
;; (define r1*r2 (mul r1 r2))
;; (define r1/r2 (div r1 r2))
;;
;; (type-tag r1+r2) ; rational
;; (contents r1+r2) ; (13 . 10)
;;
;; (type-tag r1-r2) ; rational
;; (contents r1-r2) ; (-3 . 10)
;;
;; (type-tag r1*r2) ; rational
;; (contents r1*r2) ; (2 . 5)
;;
;; (type-tag r1/r2) ; rational
;; (contents r1/r2) ; (5 . 8)


;; Tests 3: Complex numbers
;;
;; (define z1 (make-complex-from-real-imag 5 3))
;; (define z2 (make-complex-from-mag-ang 10 20))
;;
;; (type-tag z1) ; complex
;; (contents z1) ; (rectangular 5 . 3)
;;
;; (type-tag z2) ; complex
;; (contents z2) ; (polar 10 . 20)
;;
;; (define z1+z2 (add z1 z2))
;; (define z1-z2 (sub z1 z2))
;; (define z1*z2 (mul z1 z2))
;; (define z1/z2 (div z1 z2))
;;
;; (type-tag z1+z2) ; complex
;; (contents z1+z2) ; (rectangular 9.08082061813392 . 12.129452507276277)
;;
;; (type-tag z1-z2) ; complex
;; (contents z1-z2) ; (rectangular .9191793818660807 . -6.129452507276277)
;;
;; (type-tag z1*z2) ; complex
;; (contents z1*z2) ; (polar 58.309518948453004 . 20.540419500270584)
;;
;; (type-tag z1/z2) ; complex
;; (contents z1/z2) ; (polar .5830951894845301 . -19.459580499729416)


;; Combining Data of Different Types
;;
;; The operations we have defined so far treat the different data types
;; as being completely independent.
;;
;; What we have not yet considered is the fact that it is meaningful
;; to define operations that cross the type boundaries,
;; such as the addition of a complex number to an ordinary number.
;;
;; We would like to introduce the cross-type operations
;; in some carefully controlled way, so that we can support them
;; without seriously violating our module boundaries.

;; One way to handle cross-type operations is to design a different procedure
;; for each possible combination of types for which the operation is valid.
;;
;; For example, we could extend the complex-number package so that
;; it provides a procedure for adding complex numbers to ordinary numbers
;; and installs this in the table using the tag (complex scheme-number):

;; to be included in the complex package
;; (define (add-complex-to-schemenum z x)
;;   (make-from-real-imag (+ (real-part z) x)
;;                        (imag-part z)))
;; (put 'add '(complex scheme-number)
;;      (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; This technique works, but it is cumbersome.
;; With such a system, the cost of introducing a new type
;; is not just the construction of the package of procedures for that type
;; but also the construction and installation of the procedures that
;; implement the cross-type operations.


;; Coercion
;;
;; In the general situation of completely unrelated operations
;; acting on completely unrelated types, implementing explicit cross-type operations,
;; cumbersome though it may be, is the best that one can hope for.
;;
;; Often the different data types are not completely independent,
;; and there may be ways by which objects of one type may be viewed as
;; being of another type. This process is called 'coercion'.
;;
;; In general, we can implement this idea by designing coercion procedures that
;; transform an object of one type into an equivalent object of another type.

;; Here is a typical coercion procedure, which transforms a given ordinary number
;; to a complex number with that real part and zero imaginary part:

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; We install these coercion procedures in a special coercion table,
;; indexed under the names of the two types:
;;
;; (put-coercion 'scheme-number 'complex scheme-number->complex)

(define sn->complex-coercions
  (list (export-def 'scheme-number 'complex
                    scheme-number->complex)))

;; From ex-2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

;; Once the coercion table has been set up, we can handle coercion
;; in a uniform manner by modifying the apply-generic procedure.
;;
;; For simplicity, we consider only the case where there are
;; two arguments.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


;; (define n3 3)
;; (define z3 (make-complex-from-real-imag 4 5))
;;
;; (add n3 z3) ; (complex rectangular 7 . 5)
;; (sub z3 n3) ; (complex rectangular 1 . 5)
;; (mul z3 n3) ; (complex polar 19.209372712298546 . .8960553845713439)
;; (div z3 n3) ; (complex polar 2.1343747458109497 . .8960553845713439)


(define (get-coercion type1 type2)
  (get-export-def type1 type2
                  sn->complex-coercions))


;; Although we still need to write coercion procedures to relate the types,
;; we need to write only one procedure for each pair of types rather than
;; a different procedure for each collection of types and each generic operation.
;;
;; On the other hand, there may be applications for which our coercion scheme
;; is not general enough.
;; Even when neither of the objects to be combined can be converted to
;; the type of the other it may still be possible to perform the operation
;; by converting both objects to a third type.


;; Hierarchies of types
;;
;; Tower structure
;;
;; If we have a tower structure,
;; then we can greatly simplify the problem of adding a new type
;; to the hierarchy, for we need only specify how the new type
;; is embedded in the next supertype above it and
;; how it is the supertype of the type below it.


;; Example: Symbolic Algebra
;;
;; The manipulation of symbolic algebraic expressions is a complex process
;; that illustrates many of the hardest problems that occur in the design
;; of large-scale systems.
;;
;; An algebraic expression, in general, can be viewed as a hierarchical structure,
;; a tree of operators applied to operands. We can construct algebraic expressions
;; by starting with a set of primitive objects, such as constants and variables,
;; and combining these by means of algebraic operators, such as addition and
;; multiplication.

;; For example, we could describe the expression
;;
;; x^2 * sin(y^2 + 1) + x * cos(2y) + cos(y^3 - 2y^2)
;;
;; as a polynomial in x with coefficients that are trigonometric functions of
;; polynomials in y whose coefficients are integers.


;; Arithmetic on polynomials
;;
;; The first task in designing a system for performing arithmetic on polynomials
;; is to decide just what a polynomial is.
;;
;; Polynomials are normally defined relative to certain variables
;; (the indeterminates of the polynomial).
;; For simplicity, we will restrict ourselves to polynomials
;; having just one indeterminate (univariate polynomials).
;;
;; A polynomial will be defined to be a sum of terms, each of which
;; is either a coefficient, a power of the indeterminate, or a product
;; of a coefficient and a power of the indeterminate.
;;
;; For example,
;;
;; 5x^2 + 3x + 7
;;
;; is a simple polynomial in x, and
;;
;; (y^2 + 1) * x^3 + 2y * x + 1
;;
;; is a polynomial in x whose coefficients are polynomials in y.
;;
;; It's also been decided that in our algebraic-manipulation system
;; a 'polynomial' will be a particular syntactic form,
;; not its underlying mathematical meaning.
;;
;; Moreover, we will insist that two polynomials to be combined
;; must have the same indeterminate.


;; Polynomials representation
;;
;; Polynomials will be represented using a data structure called a 'poly',
;; which consists of a variable and a collection of terms.


;; Addition and multiplication of polys

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))


;; Extend the generic arithmetic system with polynomials
;;
;; To incorporate polynomials into our generic arithmetic system,
;; we need to supply them with type tags.
;; We'll use the tag 'polynomial', and install appropriate operations
;; on tagged polynomials in the operation table.
;;
;; The code will be embedded in an installation procedure for
;; the polynomial package.

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  <procedures same-variable? and variable? from section 2.3.2>
  ;; representation of terms and term lists
  <procedures adjoin-term ...coeff from text below>
  (define (add-poly p1 p2) ...)
  <procedures used by add-poly>
  (define (mul-poly p1 p2) ...)
  <procedures used by mul-poly>
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; Polynomial addition

(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms l1 (rest-terms l2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms l1)
                              (rest-terms l2)))))))))

;; The most important point to note here is that we used
;; the generic addition procedure 'add' to add together
;; the coefficients of the terms being combined.


;; Polynomial multiplication

(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-items (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

(define (mul-term-by-all-items t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-items t1 (rest-terms l))))))

;; Notice that, since we operate on terms using the generic procedures
;; 'add' and 'mul', our polynomial package is automatically able
;; to handle any type of coefficient that is known about
;; by the generic arithmetic package.


;; Representing term lists
;;
;; A term list is, in effect, a set of coefficients keyed by
;; the order of the term. Hence, any of the methods for representing sets,
;; as discussed in section 2.3.3, can be applied to this task.
;;
;; On the other hand, the procedures add-terms and mul-terms always access
;; term lists sequentially from highest to lowest order.
;; Thus, we will use some kind of ordered list representation.


;; Dense and sparse polynomials
;;
;; A polynomial is said to be 'dense' if it has nonzero coefficients
;; in terms of most orders. If it has many zero terms it is said to be sparse.
;;
;; For example,
;;
;; A: x^5 + 2x^4 + 3x^2 - 2x - 5
;;
;; is a dense polynomial, whereas
;;
;; B: x^100 + 2x^2 + 1
;;
;; is sparse.
;;
;; The term lists of dense polynomials are most efficiently represented as
;; lists of the coefficients.
;; For example, A above would be nicely represented as (1 2 0 3 -2 -5).
;;
;; A more reasonable representation of the term list of a sparse polynomial
;; is as a list of the nonzero terms, where each term is a list containing
;; the order of the term and the coefficient for that order.
;; Hence, polynomial B is efficiently represented as ((100 1) (2 2) (0 1)).


(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
