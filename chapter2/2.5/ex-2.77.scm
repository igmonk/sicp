;; Exercise 2.77
;;
;; Louis Reasoner tries to evaluate the expression (magnitude z)
;; where z is the object shown in figure 2.24.
;;
;; To his surprise, instead of the answer 5 he gets an error message
;; from apply-generic, saying there is no method for the operation magnitude
;; on the types (complex).
;;
;; He shows this interaction to Alyssa P. Hacker, who says
;; 'The problem is that the complex-number selectors were never defined for
;; complex numbers, just for polar and rectangular numbers.
;; All you have to do to make this work is add the following to the complex package':

(load "workbook.scm")

(define additional-complex-exports
  (list (export-def 'real-part '(complex) real-part)
        (export-def 'imag-part '(complex) imag-part)
        (export-def 'magnitude '(complex) magnitude)
        (export-def 'angle '(complex) angle)))

;; Describe in detail why this works.
;; As an example, trace through all the procedures called in evaluating
;; the expression (magnitude z) where z is the object shown in figure 2.24.
;;
;; In particular, how many times is apply-generic invoked?
;; What procedure is dispatched to in each case?

;; Here, another layer of abstraction is build over the existing
;; set of procedures (real-part, imag-part, magnitude and angle)
;; defined for both rectangular and polar complex number representations.
;;
;; These new procedures were defined for the '(complex) type and
;; added to the dispatch table.
;;
;; Whenever one of these procedures is invoked, a two-level dispatch process
;; takes place and the tag is stripped off twice:
;; 1) at the level of a general complex number
;; 2) at the level of a concrete complex number representation
;;
;; Hence, by calling the 'magnitude' procedure on a complex number,
;; apply-generic strips off the tag '(complex) and dispatches the contents
;; to the procedure 'magnitude' defined for the '(complex) type.
;;
;; Next, apply-generic is called again and at this time it strips off
;; the tag '(rectangular) or '(polar) depending of the concrete complex
;; number type, and dispatches the contents to the procedure 'magnitude'
;; defined for the corresponding concrete complex number type.


;; Tests
;;
;; (define z1 (make-complex-from-real-imag 5 3))
;; (define z2 (make-complex-from-mag-ang 10 20))
;;
;; (real-part z1) ; 5
;; (imag-part z1) ; 3
;; (magnitude z1) ; 5.830951894845301
;; (angle z1)     ; .5404195002705842
;;
;; (real-part z2) ; 4.080820618133919
;; (imag-part z2) ; 9.129452507276277
;; (magnitude z2) ; 10
;; (angle z2)     ; 20

;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append scheme-number-package-exports
                           rational-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           additional-complex-exports)))
