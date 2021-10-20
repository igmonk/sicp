;; Exercise 2.78
;;
;; The internal procedures in the scheme-number package are essentially
;; nothing more than calls to the primitive procedures +, -, etc.
;;
;; It was not possible to use the primitives of the language directly
;; because our type-tag system requires that each data object have a type attached to it.
;;
;; In fact, however, all Lisp implementations do have a type system,
;; which they use internally.
;;
;; Primitive predicates such as symbol? and number? determine whether
;; data objects have particular types.
;;
;; Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2
;; so that our generic system takes advantage of Scheme's internal type system.
;; That is to say, the system should work as before except that ordinary numbers
;; should be represented simply as Scheme numbers rather than as pairs
;; whose car is the symbol scheme-number.

(load "workbook.scm")

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


;; Tests
;;
;; (define n1 (make-scheme-number 10))
;; (define n2 (make-scheme-number 20))
;;
;; n1 ; 10
;; n2 ; 20
;;
;; (type-tag n1) ; scheme-number
;; (type-tag n2) ; scheme-number
;; (type-tag 21) ; scheme-number
;;
;; (contents n1) ; 10
;; (contents n2) ; 20
;; (contents 21) ; 21
;;
;; (add n1 n2) ; 30
;; (sub n1 n2) ; -10
;; (mul n1 n2) ; 200
;; (div n1 n2) ; 1/2
;;
;; (add 21 10) ; 31
;; (sub 21 10) ; 11
;; (mul 21 10) ; 210
;; (div 21 10) ; 21/10
