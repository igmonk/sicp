;; Exercise 2.81
;;
;; Louis Reasoner has noticed that apply-generic may try to coerce the arguments
;; to each other's type even if they already have the same type.
;; Therefore, he reasons, we need to put procedures in the coercion table
;; to "coerce" arguments of each type to their own type.
;;
;; For example, in addition to the scheme-number->complex coercion shown above,
;; he would do:

(load "../2.4/export-defs.scm")
(load "workbook.scm")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(define louis-coercions
  (list (export-def 'scheme-number 'scheme-number
                    scheme-number->scheme-number)
        (export-def 'complex 'complex complex->complex)))


;; a. With Louis's coercion procedures installed,
;;    what happens if apply-generic is called with two arguments
;;    of type scheme-number or two arguments of type complex
;;    for an operation that is not found in the table for those types?
;;
;; For example, assume that we've defined a generic exponentiation operation:

(define (exp x y) (apply-generic 'exp x y))

;; and have put a procedure for exponentiation in the Scheme-number package
;; but not in any other package:

(define louis-exports
  (list (export-def 'exp '(scheme-number scheme-number)
                    (lambda (x y)
                      (attach-tag 'scheme-number (expt x y))))))

;; What happens if we call exp with two complex numbers as arguments?


;; Without adding the exp procedure to the scheme-number package,
;; calling exp with only scheme numbers or only complex numbers
;; ends up in the endless recursion.
;;
;; After the exp procedure has been added to the dispatch table
;; for scheme numbers, the call to exp with only scheme numbers
;; starts to be working as expected:
;;
;; (exp 2 3) ; 8
;;
;; Calling exp with only complex numbers, however, results as before -
;; it runs into the endless recursion.
;;
;; The reason for that is apply-generic is constantly trying to
;; find a procedure to be applied to the given arguments of type given,
;; and, after such an operation is not found, coercing the arguments
;; to the type that they already have.
;; And, since the coercion table will always return
;; the complex->complex coercion procedure, apply-generic has no chances
;; to stop invoking itself, until an appropriate procedure if defined
;; for complex numbers and put into the dispatch table.


;; b. Is Louis correct that something had to be done
;;    about coercion with arguments of the same type,
;;    or does apply-generic work correctly as is?


;; As the task c. suggests,
;; apply-generic can be modified so that it doesn't try coercion
;; if the two arguments have the same type.
;; The coercion to the same type won't help as long as
;; there is no operation defined in the dispatch table.


;; c. Modify apply-generic so that it doesn't try coercion if
;;    the two arguments have the same type.


(define (apply-generic op . args)
  (define (no-method-found op type-tags)
    (error "No method for these types"
           (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (no-method-found op type-tags)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (no-method-found op type-tags))))))
              (no-method-found op type-tags))))))


;; (exp 2 3) ; 8
;;
;; (exp (make-complex-from-real-imag 2 3)
;;      (make-complex-from-real-imag 2 4))
;;
;; No method for these types (exp (complex complex))


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append scheme-number-package-exports
                           rational-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           louis-exports)))

(define (get-coercion type1 type2)
  (get-export-def type1 type2
                  (append sn->complex-coercions
                          louis-coercions)))
