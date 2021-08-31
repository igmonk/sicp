;; Exercise 2.6
;;
;; In case representing pairs as procedures wasn't mind-boggling [ошеломляющий] enough,
;; [the mind boggles = уму непостижимо]
;; consider that, in a language that can manipulate procedures,
;; we can get by without numbers (at least insofar as nonnegative integers are concerned)
;; by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;; ((zero inc) 0) ; 0
;; ((zero inc) 1) ; 1
;; ((zero inc) 2) ; 2
;; ((zero inc) 3) ; 3

;; (((add-1 zero) inc) 0) ; 1
;; (((add-1 zero) inc) 1) ; 2
;; (((add-1 zero) inc) 2) ; 3
;; (((add-1 zero) inc) 3) ; 4

;; Church numerals:
;;   n is represented as n applications of the function f to the argument x.
;;   This representation is refered to as CHURCH NUMERALS.
;;
;; zero  = Lf.Lx.x
;; one   = Lf.Lx.(f x)
;; two   = Lf.Lx.(f (f x))
;; three = Lf.Lx.(f (f (f x)))
;; four  = Lf.Lx.(f (f (f (f x))))
;;
;; next = Ln.Lf.Lx.(f ((n f) x))


;; Define one and two directly (not in terms of zero and add-1).
;; (Hint: Use substitution to evaluate (add-1 zero)).

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

;; ((one inc) 0) ; 1
;; ((one inc) 1) ; 2
;; ((one inc) 2) ; 3
;; ((one inc) 3) ; 4

;; (((add-1 one) inc) 0) ; 2
;; (((add-1 one) inc) 1) ; 3
;; (((add-1 one) inc) 2) ; 4
;; (((add-1 one) inc) 3) ; 5

;; Substitution for (add-1 zero):
;;
;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;;
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) x)) f) x))))
;;
;; >---: ((lambda (f) (lambda (x) x)) f) -> (lambda (x) x) :---<
;;
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (x) x) x))))
;;
;; >---: ((lambda (x) x) x) -> x :---<

;; (lambda (f)
;;   (lambda (x)
;;     (f x)))


(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; ((two inc) 0) ; 2
;; ((two inc) 1) ; 3
;; ((two inc) 2) ; 4
;; ((two inc) 3) ; 5

;; (((add-1 two) inc) 0) ; 3
;; (((add-1 two) inc) 1) ; 4
;; (((add-1 two) inc) 2) ; 5
;; (((add-1 two) inc) 3) ; 6

;; Substitution for (add-1 one):
;;
;; (add-1 one)
;; (add-1 (lambda (f) (lambda (x) (f x))))
;;
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) (f x))) f) x))))
;;
;; >---: ((lambda (f) (lambda (x) (f x))) f) -> (lambda (x) (f x)) :---<
;;
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (x) (f x)) x))))
;;
;; >---: ((lambda (x) (f x)) x) -> (f x) :---<
;;
;; (lambda (f)
;;   (lambda (x)
;;     (f (f x))))


;; Give a direct definition of the addition procedure +
;; (not in terms of repeated application of add-1).

(define (plus m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;; Lambda expression: λm.λn.λf.λx.(m f)((n f) x))

;; (((plus one two) inc) 0) ; 3
;; (((plus one two) inc) 1) ; 4
;; (((plus one two) inc) 2) ; 5
;; (((plus one two) inc) 3) ; 6

;; (((plus one (plus one two)) inc) 0)            ; 4
;; (((plus (add-1 one) (plus one one)) inc) 0)    ; 4
;; (((plus (plus two two) (plus two two)) inc) 0) ; 8

;; Utils

(define (inc n) (+ n 1))


;; Resources:
;;
;; Lambda calculus:
;;   https://en.wikipedia.org/wiki/Lambda_calculus
;;
;; Church numerals:
;;   https://en.wikipedia.org/wiki/Church_encoding#Church_numerals
;;   https://www.cs.unc.edu/~stotts/723/Lambda/church.html
;;
;; Natural numbers as Church numerals:
;;   https://www.cs.unc.edu/~stotts/723/Lambda/church.html
