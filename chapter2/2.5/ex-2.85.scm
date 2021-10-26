;; Exercise 2.85
;;
;; This section mentioned a method for 'simplifying' a data object
;; by lowering it in the tower of types as far as possible.
;;
;; Design a procedure 'drop' that accomplishes this for the tower
;; described in exercise 2.83.
;;
;; The key is to decide, in some general way, whether an object can be lowered.
;;
;; For example,
;; - the complex number 1.5 + 0i can be lowered as far as real,
;; - the complex number 1 + 0i can be lowered as far as integer,
;; - the complex number 2 + 3i cannot be lowered at all.
;;
;; Here is a plan for determining whether an object can be lowered:
;; Begin by defining a generic operation 'project' that
;; 'pushes' an object down in the tower.
;;
;; For example, projecting a complex number would involve
;; throwing away the imaginary part. Then a number can be dropped if,
;; when we 'project' it and 'raise' the result back to the type we started with,
;; we end up with something equal to what we started with.
;;
;; Show how to implement this idea in detail, by writing a 'drop' procedure
;; that drops an object as far as possible.
;;
;; You will need to design the various projection operations and
;; install 'project' as a generic operation in the system.
;;
;; You will also need to make use of a generic equality predicate,
;; such as described in exercise 2.79.
;;
;; Finally, use 'drop' to rewrite 'apply-generic' from exercise 2.84
;; so that it 'simplifies' its answers.

(load "../export-defs.scm")
(load "workbook.scm")
(load "ex-2.79.scm")
(load "ex-2.83.scm")
(load "ex-2.84.scm")


;; As before, start with the generic operation 'project':

(define (project x) (apply-generic 'project x))


;; Next, install package extensions for each concrete
;; number representation (except integer):


;; 1. Rational numbers

(define (install-rational-ext-2-85)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (project x)
    (round (/ (numer x) (denom x))))
  ;; interface to rest of the system
  (list (export-def 'project '(rational) project)))

(define rational-ext-2-85-exports
  (install-rational-ext-2-85))


;; 2. Real numbers

(define (install-real-ext-2-85)
  ;; internal procedures
  (define (project x)
    (let ((scheme-rat (inexact->exact x)))
      (make-rational (numerator scheme-rat)
                     (denominator scheme-rat))))
  ;; interface to rest of the system
  (list (export-def 'project '(real) project)))

(define real-ext-2-85-exports
  (install-real-ext-2-85))


;; 3. Complex numbers

(define (install-complex-ext-2-85)
  ;; internal procedures
  (define (project z) (real-part z))
  ;; interface to rest of the system
  (list (export-def 'project '(complex) project)))

(define complex-ext-2-85-exports
  (install-complex-ext-2-85))


;; Next comes the implementation of the procedure drop.
;;
;; The argument is returned immediately if 'project' is not defined
;; for the given argument. Otherwise:
;; 
;; apply 'project' to the argument to get its projected value,
;; then raise it back and compare the result with the original value
;; applying the following logic:
;; - equal?     => go on with calling drop recursively
;;                 passing the projected value
;; - not equal? => return the argument

(define (drop x)
  (if (get-project-fn (type-tag x))
      (let ((projected-x (project x)))
        (if (equ? x (raise projected-x))
            (drop projected-x)
            x))
      x))


;; (define z1 (make-complex-from-real-imag 5 1))
;; (define z2 (make-complex-from-real-imag 5 0))
;; (define real1 5.1)
;; (define real2 5.0)
;; (define rat1 (make-rational 1 5))
;; (define rat2 (make-rational 5 1))

;; (project z1)    ; 5
;; (project z2)    ; 5
;; (project real1) ; (rational 2871044762448691 . 562949953421312)
;; (project real2) ; (rational 5 . 1)
;; (project rat1)  ; 0
;; (project rat2)  ; 5

;; (drop z1)    ; (complex rectangular 5 . 1)
;; (drop z2)    ; 5
;; (drop real1) ; (rational 2871044762448691 . 562949953421312)
;; (drop real2) ; 5
;; (drop rat1)  ; (rational 1 . 5)
;; (drop rat2)  ; 5


;; Now it is time to redefine apply-generic.
;; The only adjustment its latest version (ex. 2.84) should obtain
;; is the invocation of the procedure drop when applying
;; the given operation to the arguments.
;;
;; Notice, when the operation is of type 'raise' or 'project',
;; the procedure drop is not applied, otherwise the program ends up
;; in the endless recursion.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            (if (safe-to-drop? op) (drop result) result))
          (if (all-equal? type-tags)
              (if (first-raisable? type-tags)
                  (apply apply-generic (cons op (map raise args)))
                  (error "No method for these types"
                         (list op type-tags)))
              (apply apply-generic (cons op (raise-to-the-highest args))))))))

(define (safe-to-drop? proc)
  (and (not (equal? proc 'raise))
       (not (equal? proc 'project))))


;; (add5 1 2 3 4 5) ; 15
;; (mul5 1 2 3 4 5) ; 120

;; (add5 1 2 3 4 rat1) ; (rational 51 . 5)
;; (mul5 1 2 3 4 rat1) ; (rational 24 . 5)

;; (add5 1 2 3 rat1 real1) ; (rational 3180667236830413 . 281474976710656)
;; (mul5 1 2 3 rat1 real1) ; (rational 6890507429876859 . 1125899906842624)

;; The following code fails due to the complex package does not support
;; rational components (real/imag/mag/ang parts) yet.
;; The lack of support becomes evident when the procedure drop is applied.
;; See exercise 2.86.
;;
;; (add5 1 2 rat1 real1 z1) ; FAILS
;; (mul5 1 2 rat1 real1 z1) ; FAILS

;; (add5 z1 z1 z1 z1 z1) ; (complex rectangular 25 . 5)
;; (add5 1 2 3 4 z1)     ; (complex rectangular 15 . 1)


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append integer-package-exports
                           integer-ext-2-84-exports
                           rational-package-exports
                           rational-ext-2-79-exports
                           rational-ext-2-83-exports
                           rational-ext-2-84-exports
                           rational-ext-2-85-exports
                           real-package-exports
                           real-ext-2-84-exports
                           real-ext-2-85-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           complex-ext-2-79-exports
                           complex-ext-2-84-exports
                           complex-ext-2-85-exports)))

(define (get-project-fn type)
  (get 'project (list type)))

;; From ex. 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; From ex. 2.78
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (type-tag datum)
  (cond ((exact-integer? datum) 'integer)
        ((real? datum) 'real)
        ((pair? datum) (car datum))
        ((boolean? datum) 'bool) ;; Alternatively, adjust the procedure drop.
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
