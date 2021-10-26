;; Exercise 2.84
;;
;; Using the raise operation of exercise 2.83,
;; modify the apply-generic procedure so that it coerces its arguments
;; to have the same type by the method of successive raising,
;; as discussed in this section.
;;
;; You will need to devise a way to test which of two types
;; is higher in the tower.
;;
;; Do this in a manner that is 'compatible' with the rest of the system
;; and will not lead to problems in adding new levels to the tower.

(load "../../common.scm")
(load "../export-defs.scm")
(load "workbook.scm")
(load "ex-2.78.scm")
(load "ex-2.83.scm")

;; As in ex. 2.82, we start with a couple of
;; new generic operations defined for more than 2 arguments
;; in order to check the work of apply-generic.
;;
;; As example, we can consider the following operations:
;; - add5 - calculates the sum of five numbers
;; - mul5 - calculates the product of five numbers

(define (add5 n1 n2 n3 n4 n5)
  (apply-generic 'add5 n1 n2 n3 n4 n5))

(define (mul5 n1 n2 n3 n4 n5)
  (apply-generic 'mul5 n1 n2 n3 n4 n5))

;; Next, install the add5 and mul5 procedures to
;; all the number representation packages.

;; 1. Integer package extension

(define (install-integer-ext-2-84)
  (define types-arity-5
    '(integer integer integer integer integer))
  (list (export-def 'add5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (+ n1 n2 n3 n4 n5)))
        (export-def 'mul5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (* n1 n2 n3 n4 n5)))))

(define integer-ext-2-84-exports
  (install-integer-ext-2-84))


;; 2. Rational package extension

(define (install-rational-ext-2-84)
  (define types-arity-5
    '(rational rational rational rational rational))
  (define (tag x) (attach-tag 'rational x))
  (define (add5 n1 n2 n3 n4 n5)
    (accumulate add (tag n1)
                (map tag (list n2 n3 n4 n5))))
  (define (mul5 n1 n2 n3 n4 n5)
    (accumulate mul (tag n1)
                (map tag (list n2 n3 n4 n5))))
  (list (export-def 'add5 types-arity-5 add5)
        (export-def 'mul5 types-arity-5 mul5)))

(define rational-ext-2-84-exports
  (install-rational-ext-2-84))


;; 3. Real package extension

(define (install-real-ext-2-84)
  (define types-arity-5
    '(real real real real real))
  (list (export-def 'add5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (+ n1 n2 n3 n4 n5)))
        (export-def 'mul5 types-arity-5
                    (lambda (n1 n2 n3 n4 n5)
                      (* n1 n2 n3 n4 n5)))))

(define real-ext-2-84-exports
  (install-real-ext-2-84))


;; 4. Complex package extension

(define (install-complex-ext-2-84)
  (define types-arity-5
    '(complex complex complex complex complex))
  (define (tag z) (attach-tag 'complex z))
  (define (add5 z1 z2 z3 z4 z5)
    (accumulate add (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (define (mul5 z1 z2 z3 z4 z5)
    (accumulate mul (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (list (export-def 'add5 types-arity-5 add5)
        (export-def 'mul5 types-arity-5 mul5)
        (export-def 'real-part '(complex) real-part)
        (export-def 'imag-part '(complex) imag-part)))

(define complex-ext-2-84-exports
  (install-complex-ext-2-84))


;; Now, with the wishful thinking strategy in mind,
;; we can build the updated version of apply-generic.
;;
;; As before, if there is a procedure defined for the given arguments,
;; it is applied.
;;
;; Otherwise, we perform the equality check of the argument types.
;;
;; If all the types are equal, before giving up we can check if
;; they all can be raised one level up the number hierarchy and
;; call apply-generic recursively if they can.
;;
;; If not all the types are equal, we raise all the arguments
;; to the hierarchy level of the argument at the highest one
;; and make another recursive call.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (all-equal? type-tags)
              (if (first-raisable? type-tags)
                  (apply apply-generic (cons op (map raise args)))
                  (error "No method for these types"
                         (list op type-tags)))
              (apply apply-generic (cons op (raise-to-the-highest args))))))))

(define (first-raisable? types)
  (and (not (null? types))
       (get-raise-fn (car types))))

;; Raise every number to the hierarchy level which
;; the number with the highest one resides at.
(define (raise-to-the-highest numbers)
  (map raise-n numbers (get-relative-depths numbers)))

(define (get-relative-depths numbers)
  (let ((abs-depths (get-absolute-depths numbers)))
    (let ((min-abs-depth (apply min abs-depths)))
      (map (lambda (abs-depth)
             (- abs-depth min-abs-depth))
           abs-depths))))

(define (get-absolute-depths numbers)
  (map get-absolute-depth numbers))

(define (get-absolute-depth number)
  (define (inner n result)
    (if (get-raise-fn (type-tag n))
        (inner (raise n) (inc result))
        result))
  (inner number 0))

;; Raise the argument n times up the hierarchy of types.
;; Implemented as repeated application of the procedure raise.
(define (raise-n x n)
  ((repeated raise n) x))


;; Tests
;;
;; (define int1 5)
;; (define rat1 (make-rational 1 5))
;; (define real1 5.1)
;; (define z1 (make-complex-from-real-imag 5 1))

;; (get-absolute-depth int1)  ; 3
;; (get-absolute-depth rat1)  ; 2
;; (get-absolute-depth real1) ; 1
;; (get-absolute-depth z1)    ; 0

;; (raise-n int1 1) ; (rational 5 . 1)
;; (raise-n int1 2) ; 5.
;; (raise-n int1 3) ; (complex rectangular 5. . 0)

;; (get-absolute-depths (list int1 rat1 real1 z1)) ; (3 2 1 0)

;; (get-relative-depths (list int1 rat1 real1 z1)) ; (3 2 1 0)
;; (get-relative-depths (list int1 rat1 real1))    ; (2 1 0)
;; (get-relative-depths (list int1 rat1))          ; (1 0)
;; (get-relative-depths (list int1))               ; (0)

;; (raise-to-the-highest (list 1 2 3 4 5))    ; (1 2 3 4 5)
;; (raise-to-the-highest (list 1 2 3 4 rat1)) ; ((rational 1 . 1)
;; ;                                             (rational 2 . 1)
;; ;                                             (rational 3 . 1)
;; ;                                             (rational 4 . 1)
;; ;                                             (rational 1 . 5))
;; (raise-to-the-highest (list 1 2 3 4 real1)) ; (1. 2. 3. 4. 5.1)
;; (raise-to-the-highest (list 1 2 3 4 z1))    ; ((complex rectangular 1. . 0)
;; ;                                              (complex rectangular 2. . 0)
;; ;                                              (complex rectangular 3. . 0)
;; ;                                              (complex rectangular 4. . 0)
;; ;                                              (complex rectangular 5 . 1))

;; (add5 1 2 3 4 5) ; 15
;; (mul5 1 2 3 4 5) ; 120

;; (add5 1 2 3 4 rat1) ; (rational 51 . 5)
;; (mul5 1 2 3 4 rat1) ; (rational 24 . 5)

;; (add5 1 2 3 rat1 real1) ; 11.3
;; (mul5 1 2 3 rat1 real1) ; 6.12

;; (add5 1 2 rat1 real1 z1) ; (complex rectangular 13.299999999999999 . 1)
;; (mul5 1 2 rat1 real1 z1) ; (complex polar 10.40199980772928 . .19739555984988075)

;; (real-part int1) ; 5.
;; (imag-part int1) ; 0

;; (real-part rat1) ; .2
;; (imag-part rat1) ; 0

;; (real-part real1) ; 5.1
;; (imag-part real1) ; 0

;; (real-part z1) ; 5
;; (imag-part z1) ; 1


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append integer-package-exports
                           integer-ext-2-84-exports
                           rational-package-exports
                           rational-ext-2-83-exports
                           rational-ext-2-84-exports
                           real-package-exports
                           real-ext-2-84-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           complex-ext-2-84-exports)))

(define (get-raise-fn type)
  (get 'raise (list type)))
