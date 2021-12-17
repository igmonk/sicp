;; Exercise 3.62
;;
;; Use the results of exercises 3.60 and 3.61 to define a procedure
;; 'div-series' that divides two power series.
;;
;; Div-series should work for any two series, provided that
;; the denominator series begins with a nonzero constant term.
;; (If the denominator has a zero constant term, then div-series
;; should signal an error.)
;;
;; Show how to use div-series together with the result of
;; exercise 3.59 to generate the power series for tangent.

(load "workbook.scm")
(load "ex-3.59.scm")
(load "ex-3.60.scm")
(load "ex-3.61.scm")

;; The procedure div-series multiplies the numerator series
;; by the inverted denominator series.
;;
;; In order to find the inverted denominator series,
;; the following steps need to be taken:
;; 1) Scale the denomitator series by the inverted constant term
;; 2) Feed the result to the procedure invert-unit-series
;; 3) Scale the resulting series by the inverted constant term
;;
;; Keep in mind that 'invert-unit-series' expects the constant
;; term to be equal 1. Therefore, the initial stream has to be
;; downscaled prior to invoking it and compensated later by
;; scaling the resulting series back.

(define (div-series n d)
  (let ((d-const (stream-car d)))
    (if (= 0 d-const)
        (error "Zero constant term -- DIV-SERIES")
        (mul-series n
                    (scale-stream
                     (invert-unit-series
                      (scale-stream d (/ 1 d-const)))
                     (/ 1 d-const))))))

(define tangent-series
  (div-series sine-series cosine-series))


;; Test 1: integers
;;
;; (define integers-/-integers
;;   (div-series integers integers))
;;
;; (stream-ref integers-/-integers 0) ; 1
;; (stream-ref integers-/-integers 1) ; 0
;; (stream-ref integers-/-integers 2) ; 0
;; (stream-ref integers-/-integers 3) ; 0
;; (stream-ref integers-/-integers 4) ; 0
;; (stream-ref integers-/-integers 5) ; 0, etc.


;; Test 2: sevens
;;
;; (define sevens (cons-stream 7 sevens))
;;
;; (define sevens-/-sevens
;;   (div-series sevens sevens))
;;
;; (stream-ref sevens-/-sevens 0) ; 1
;; (stream-ref sevens-/-sevens 1) ; 0
;; (stream-ref sevens-/-sevens 2) ; 0
;; (stream-ref sevens-/-sevens 3) ; 0
;; (stream-ref sevens-/-sevens 4) ; 0
;; (stream-ref sevens-/-sevens 5) ; 0


;; Test 3: tangent
;;
;; (stream-ref tangent-series 0) ; 0
;; (stream-ref tangent-series 1) ; 1
;; (stream-ref tangent-series 2) ; 0
;; (stream-ref tangent-series 3) ; 1/3
;; (stream-ref tangent-series 4) ; 0
;; (stream-ref tangent-series 5) ; 2/15
;; (stream-ref tangent-series 6) ; 0
;; (stream-ref tangent-series 7) ; 17/315
;; (stream-ref tangent-series 8) ; 0
;; (stream-ref tangent-series 9) ; 62/2835
;;
;; See:
;; https://proofwiki.org/wiki/Power_Series_Expansion_for_Tangent_Function
