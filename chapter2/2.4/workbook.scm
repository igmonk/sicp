;; 2.4 Multiple Representations for Abstract Data

;; This section shows how to cope with data that may be represented in different ways
;; by different parts of a program.
;;
;; This requires:
;; - constructing 'generic procedures' - procedures that can operate on data
;;   that may be represented in more than one way
;; - working in terms of data objects that have 'type tags', that is,
;;   data objects that include explicit information about how they are to be processed
;;   (our main technique for building generic procedures)
;; - 'data-directed' programming, a powerful and convenient implementation strategy
;;   for additively assembling systems with generic operations


;; Representations for Complex Numbers
;;
;; We begin by discussing two plausible representations for complex numbers as ordered pairs:
;;
;; 1. rectangular form (real part and imaginary part)
;; 2. polar form (magnitude and angle)

;; Thus, there are two different representations for complex numbers,
;; which are appropriate for different operations:
;;
;; 1. Addition
;;
;;    Re(z1 + z2) = Re(z1) + Re(z2)
;;    Im(z1 + z2) = Im(z1) + Im(z2)
;;
;; 2. Multiplication
;;
;;    Mag(z1 * z2) = Mag(z1) * Mag(z2)
;;    Ang(z1 * z2) = Ang(z1) + Ang(z2)

;; Assume that the operations on complex numbers are implemented in terms of four selectors:
;; 1) real-part
;; 2) imag-part
;; 3) magnitude
;; 4) and angle
;;
;; Also assume that we have two procedures for constructing complex numbers:
;; 1) make-from-real-imag
;;    returns a complex number with specified real and imaginary parts,
;; 2) make-from-mag-ang
;;    returns a complex number with specified magnitude and angle.

;; These procedures have the property that, for any complex number z, both
;;
;; (make-from-real-imag (real-part z) (imag-part z))
;;
;; and
;;
;; (make-from-mag-ang (magnitude z) (angle z))
;;
;; produce complex numbers that are equal to z.

;; Using these constructors and selectors,
;; we can implement arithmetic on complex numbers using the 'abstract data'
;; specified by the constructors and selectors:

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

;; There are two obvious ways to choose a representation and implement
;; the constructors and selectors in terms of primitive numbers
;; and primitive list structure:
;;
;; 1. in 'rectangular form' as a pair (real part, imaginary part)
;; 2. in 'polar form' as a pair (magnitude, angle)


;; 1. Rectangular form

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


;; 2. Polar form

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z)
  (car z))

(define (angle z)
  (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))


;; Tagged data
;;
;; A straightforward way to accomplish the distinction between two different
;; representations of complex numbers is to include a 'type tag' - the symbol
;; 'rectangular' or 'polar' - as part of each complex number.
;;
;; Then when we need to manipulate a complex number we can use the tag
;; to decide which selector to apply.

;; In order to manipulate tagged data,
;; we will assume that we have procedures 'type-tag' and 'contents'
;; that extract from a data object the tag and the actual contents
;; (the polar or rectangular coordinates, in the case of a complex number).
;;
;; We will also postulate a procedure 'attach-tag' that takes a tag and contents
;; and produces a tagged data object.
;;
;; A straightforward way to implement this is to use ordinary list structure:

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

;; Using these procedures, we can define predicates rectangular? and polar?,
;; which recognize polar and rectangular numbers, respectively:

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; With type tags, we can now modify the code so that
;; the two different representations can coexist in the same system.
;;
;; Whenever we construct a complex number, we tag it as either rectangular or polar.
;;
;; In addition, we must make sure that the names of the procedures do not conflict.
;; One way to do this is to append the suffix 'rectangular' or 'polar' to the name
;; of each corresponding procedure.


;; 1. Revised rectangular representation

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))


;; 2. Revised polar representation

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;; Each generic selector is implemented as a procedure that checks the tag of
;; its argument and calls the appropriate procedure for handling data of that type.
;;
;; In either case, we use contents to extract the bare, untagged datum
;; and send this to the rectangular or polar procedure as required:

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


;; Finally, we must choose whether to construct complex numbers using
;; the rectangular or polar representation.
;;
;; One reasonable choice is to construct rectangular numbers whenever we have
;; real and imaginary parts and to construct polar numbers whenever we have
;; magnitudes and angles:

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


;; The complex-number arithmetic operations (add-complex, sub-complex, mul-complex,
;; and div-complex) stay the same, because the selectors they call are generic,
;; and so will work with either representation.

;; Since each data object is tagged with its type,
;; the selectors operate on the data in a generic manner.
;; That is, each selector is defined to have a behavior that
;; depends upon the particular type of data it is applied to.

;; Notice the general mechanism for interfacing the separate representations:
;; within a given representation implementation (say, the polar package)
;; a complex number is an untyped pair (magnitude, angle).
;;
;; When a generic selector operates on a number of polar type,
;; it strips off the tag and passes the contents on to the polar package code.
;;
;; Conversely, when the package developer constructs a number for general use,
;; they tag it with a type so that it can be appropriately recognized by
;; the higher-level procedures.


;; (define c1 (make-from-real-imag 5 3))
;;
;; c1                ; (rectangular 5 . 3)
;; (type-tag c1)     ; rectangular
;; (contents c1)     ; (5 . 3)
;; (rectangular? c1) ; true
;; (polar? c1)       ; false
;;
;; (real-part c1) ; 5
;; (imag-part c1) ; 3
;; (magnitude c1) ; 5.830951894845301
;; (angle c1)     ; .5404195002705842


;; (define c2 (make-from-mag-ang 10 20))
;;
;; c2                ; (polar 10 . 20)
;; (type-tag c2)     ; polar
;; (contents c2)     ; (10 . 20)
;; (rectangular? c2) ; false
;; (polar? c2)       ; true
;;
;; (real-part c2) ; 4.080820618133919
;; (imag-part c2) ; 9.129452507276277
;; (magnitude c2) ; 10
;; (angle c2)     ; 20


;; Data-Directed Programming and Additivity
;;
;; Implementing the dispatch as in the previous section has two significant weaknesses:
;; 1. the generic interface procedures (real-part, imag-part, magnitude, and angle)
;;    must know about all the different representations
;; 2. even though the individual representations can be designed separately,
;;    we must guarantee that no two procedures in the entire system have the same name
;;
;; What we need is a means for modularizing the system design even further.
;; This is provided by the programming technique known as data-directed programming.
;;
;; Whenever we deal with a set of generic operations that are common to a set of
;; different types we are, in effect, dealing with a two-dimensional table that
;; contains the possible operations on one axis and the possible types on the other axis.
;; The entries in the table are the procedures that implement each operation
;; for each type of argument presented.
;;
;; |------------|-----------------------------------------|
;; |            |               Types                     |
;; | Operations |-----------------------------------------|
;; |            |       Polar     |       Rectangular     |
;; |------------|-----------------|-----------------------|
;; | real-part  | real-part-polar | real-part-rectangular |
;; | imag-part  | imag-part-polar | imag-part-rectangular |
;; | magnitude  | magnitude-polar | magnitude-rectangular |
;; | angle      | angle-polar     | angle-rectangular     |
;; |------------|-----------------------------------------|
;;
;; The interface is implemented as a single procedure that looks up
;; the combination of the operation name and argument type in the table
;; to find the correct procedure to apply, and then applies it
;; to the contents of the argument.

;; To implement this plan, assume that we have two procedures, 'put' and 'get',
;; for manipulating the operation-and-type table:
;;
;; (put <op> <type> <item>) --> installs the <item> in the table,
;;                              indexed by the <op> and the <type>
;;
;; (get <op> <type>)        --> looks up the <op>, <type> entry in the table
;;                              and returns the item found there.
;;                              If no item is found, 'get' returns false.
;;
;; (see chapter 3, section 3.3.3, ex. 3.24, on how to implement these operations.)

;; A particular representation is defined as a collection of procedures, or a 'package',
;; which are interfaced to the rest of the system by adding entries to the table
;; that tell the system how to operate on the given data type.


;; 1. Rectangular complex numbers package

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
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag
       'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; 2. Polar complex numbers package

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
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-mag-ang
       'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'make-from-real-imag
       'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)


;; The complex-arithmetic selectors access the table by means of
;; a general 'operation' procedure called 'apply-generic', which
;; applies a generic operation to some arguments.
;;
;; apply-generic looks in the table under the name of the operation and the types
;; of the arguments and applies the resulting procedure if one is present.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;; apply-generic also uses the primitive procedure apply, which
;; takes two arguments, a procedure and a list.
;;
;; apply applies the procedure, using the elements in the list as arguments.
;; For example,
;;
;; (apply + (list 1 2 3 4)) ; 10


;; Using apply-generic, we can define our generic selectors as follows:

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; These generic selectors do not change at all
;; if a new representation is added to the system.

;; We can also extract from the table the constructors to be used by the programs
;; external to the packages in making complex numbers from real and imaginary parts
;; and from magnitudes and angles.

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; Message passing
;;
;; An alternative implementation strategy is to decompose the operation-and-type table
;; into columns and, instead of using 'intelligent operations' that dispatch on data types,
;; to work with 'intelligent data objects' that dispatch on operation names.
;;
;; We can do this by arranging things so that a data object, such as a rectangular number,
;; is represented as a procedure that takes as input the required operation name
;; and performs the operation indicated.
;;
;; In such a discipline, make-from-real-imag could be written as

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; The corresponding 'apply-generic' procedure,
;; which applies a generic operation to an argument,
;; now simply feeds the operation's name to the data object
;; and lets the object do the work:

(define (apply-generic op arg)
  (arg op))


;; (define z1 (make-from-real-imag 5 3))
;;
;; (real-part z1) ; 5
;; (imag-part z1) ; 3
;; (magnitude z1) ; 5.830951894845301
;; (angle z1)     ; .5404195002705842


;; This style of programming is called 'message passing'.
;; The name comes from the image that a data object is an entity that receives
;; the requested operation name as a 'message'.
;;
;; Message passing is a useful technique for organizing systems with generic operations.
;;
;; One limitation of this organization is
;; it permits only generic procedures of one argument.
