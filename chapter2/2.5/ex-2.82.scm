;; Exercise 2.82
;;
;; Show how to generalize apply-generic to handle coercion
;; in the general case of multiple arguments.
;;
;; One strategy is to attempt to coerce all the arguments
;; to the type of the first argument, then to the type of the second argument,
;; and so on.
;;
;; Give an example of a situation where this strategy
;; (and likewise the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable
;; mixed-type operations present in the table that will not be tried.)

(load "../../common.scm")
(load "../2.4/export-defs.scm")
(load "workbook.scm")
(load "ex-2.78.scm")


;; In order to check the work of apply-generic,
;; start with a couple of new generic operations defined
;; for more than 2 arguments.
;;
;; As example, we can consider the following operations:
;; - add5 - calculates the sum of five numbers
;; - mul5 - calculates the product of five numbers

(define (add5 n1 n2 n3 n4 n5)
  (apply-generic 'add5 n1 n2 n3 n4 n5))

(define (mul5 n1 n2 n3 n4 n5)
  (apply-generic 'mul5 n1 n2 n3 n4 n5))


;; Install the add5 and mul5 procedures to the scheme-number package:

(define (install-scheme-number-ext-2-82)
  (define (types-arity-5)
    '(scheme-number scheme-number scheme-number scheme-number scheme-number))
  (list (export-def 'add5 (types-arity-5)
                    (lambda (n1 n2 n3 n4 n5)
                      (attach-tag 'scheme-number
                                  (+ n1 n2 n3 n4 n5))))
        (export-def 'mul5 (types-arity-5)
                    (lambda (n1 n2 n3 n4 n5)
                      (attach-tag 'scheme-number
                                  (* n1 n2 n3 n4 n5))))))

(define scheme-number-ext-2-82-exports
  (install-scheme-number-ext-2-82))


;; Install the add5 procedure to the complex package.
;;
;; A complex number has to be tagged to enable the reuse of
;; the procedures defined for the complex type.

(define (install-complex-ext-2-82)
  (define (types-arity-5)
    '(complex complex complex complex complex))
  (define (tag z) (attach-tag 'complex z))
  (define (add5 z1 z2 z3 z4 z5)
    (accumulate add (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (define (mul5 z1 z2 z3 z4 z5)
    (accumulate mul (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (list (export-def 'add5 (types-arity-5) add5)
        (export-def 'mul5 (types-arity-5) mul5)))

(define complex-ext-2-82-exports
  (install-complex-ext-2-82))


;; (add5 1 2 3 4 5) ; 15
;; (mul5 1 2 3 4 5) ; 120
;;
;; (define z12 (make-complex-from-real-imag 1 2))
;; (define z34 (make-complex-from-real-imag 3 4))
;; (define z56 (make-complex-from-real-imag 5 6))
;; (define z78 (make-complex-from-real-imag 7 8))
;; (define z90 (make-complex-from-real-imag 9 0))
;;
;; (add5 z12 z34 z56 z78 z90) ; (complex rectangular 25 . 20)
;; (mul5 z12 z34 z56 z78 z90) ; (complex polar 8354.138196127713 . 3.762468313567168)


;; Without applying any changes to the apply-generic procedure,
;; trying to sum up numbers of different types:
;;
;; (add5 1 2 3 z78 z90)
;;
;; results in the following error:
;;
;; No method for these types
;;   (add5 (scheme-number scheme-number scheme-number complex complex))


;; Now we are ready to adjust apply-generic to account for the case
;; when scheme numbers are sumed up with complex ones.

(define (apply-generic op . args)
  (define (no-method-found op type-tags)
    (error "No method for these types"
           (list op type-tags)))
  (define (contains-empty-slots? seq)
    (cond ((null? seq) false)
          ((or (null? (car seq)) (not (car seq))) true)
          (else
           (contains-empty-slots? (cdr seq)))))
  (define (get-coercion-fns-1 target-tag origin-tags)
    (map (lambda (origin-tag)
           (if (equal? origin-tag target-tag)
               identity
               (get-coercion origin-tag target-tag)))
         origin-tags))
  (define (get-coercion-fns type-tags)
    (define (inner target-tags origin-tags)
      (if (null? target-tags)
          '()
          (let ((coercion-fns (get-coercion-fns-1 (car target-tags) origin-tags)))
            (if (contains-empty-slots? coercion-fns)
                (inner (cdr target-tags) origin-tags)
                coercion-fns))))
    (inner type-tags type-tags))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (all-equal? type-tags)
              (no-method-found op type-tags)
              (let ((coercion-fns (get-coercion-fns type-tags)))
                (if (null? coercion-fns)
                    (no-method-found op type-tags)
                    (let ((coerced-args (map apply2 coercion-fns args)))
                      (apply apply-generic (cons op coerced-args))))))))))


;; (add5 1 2 3 z78 z90) ; (complex rectangular 22 . 8)
;; (mul5 1 2 3 z78 z90) ; (complex polar 574.0278738876711 . .8519663271732721)


;; This strategy (and likewise the two-argument version)
;; is not sufficiently general for two reasons:
;;
;; 1) it only accounts for direct coercion
;;
;;    If, however, one type can be coerced to another one indirectly
;;    through the coercion to a type (or a sequence of types) in between
;;    (at least two coercion operations are needed),
;;    the strategy won't realise that.
;;
;;    For ex., given the presence of the following coercion chain:
;;    - integer->rational
;;    - rational->real
;;    - real->complex
;;    the combination '(integer rational) will be working,
;;    whereas '(integer complex) won't.
;;
;; 2) it only accounts for the types involved
;;
;;    That drawback becomes clearly visible if we work with
;;    a structure that is not a tower:
;;
;;    '(Rectangle Rhombus) won't be working, since the arguments
;;    are not directly compatible with each other.
;;    However, they can still be coerced to the type of their
;;    ancestor, which is Parallelogram.
;;
;; Due to the disadvantages describe above, some suitable mixed-type operations
;; present in the table will not be tried.


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append scheme-number-package-exports
                           rational-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           scheme-number-ext-2-82-exports
                           complex-ext-2-82-exports)))

(define (all-equal? type-tags)
  (cond ((null? type-tags) true)
        ((not (pair? type-tags)) true)
        ((not (pair? (cdr type-tags))) true)
        (else
         (if (equal? (car type-tags) (cadr type-tags))
             (all-equal? (cdr type-tags))
             false))))

(define (apply2 x y) (x y))
