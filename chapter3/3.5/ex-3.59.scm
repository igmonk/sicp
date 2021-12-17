;; Exercise 3.59
;;
;; In section 2.5.3 we saw how to implement a polynomial arithmetic system
;; representing polynomials as lists of terms.
;;
;; In a similar way, we can work with power series, such as
;;
;; e^x = 1 + x + (x^2)/2 + (x^3)/(3*2) + (x^4)/(4*3*2) + ...
;;
;; cos(x) = 1 - (x^2)/2 + (x^4)/(4*3*2) - ...
;;
;; sin(x) = x - (x^3)/(3*2) + (x^5)/(5*4*3*2) - ...
;;
;; represented as infinite streams.
;;
;; We will represent the series a0 + a1 * x + a2 * x2 + a3 * x3 + ...
;; as the stream whose elements are the coefficients a0, a1, a2, a3, ...
;;
;; a. The integral of the series a0 + a1 * x + a2 * x^2 + a3 * x^3 ...
;;    is the series
;;    c + a0 * x + 1/2 * a1 * x^2 + 1/3 * a2 * x^3 + 1/4 * a3 * x^4 + ...
;;    where c is any constant.
;;
;;    Define a procedure 'integrate-series' that takes as input
;;    a stream a0, a1, a2, ... representing a power series and returns
;;    the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of
;;    the non-constant terms of the integral of the series.
;;    (Since the result has no constant term, it doesn't represent
;;    a power series; when we use integrate-series,
;;    we will cons on the appropriate constant.)

(load "workbook.scm")

(define (integrate-series s)
  (stream-map (lambda (a i)
                (* (/ 1 i) a))
              s
              integers))

;; (define fibs-int (integrate-series fibs))
;;
;; (stream-ref fibs-int 0) ; 0
;; (stream-ref fibs-int 1) ; 1/2
;; (stream-ref fibs-int 2) ; 1/3
;; (stream-ref fibs-int 3) ; 1/2 = 2/4
;; (stream-ref fibs-int 4) ; 3/5
;; (stream-ref fibs-int 5) ; 5/6
;; (stream-ref fibs-int 6) ; 8/7
;; (stream-ref fibs-int 7) ; 13/8
;; (stream-ref fibs-int 8) ; 7/3 = 21/9


;; b. The function x -> e^x is its own derivative.
;;    This implies that e^x and the integral of e^x are the same series,
;;    except for the constant term, which is e^0 = 1.
;;    Accordingly, we can generate the series for e^x as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; (stream-ref exp-series 0) ; 1
;; (stream-ref exp-series 1) ; 1
;; (stream-ref exp-series 2) ; 1/2
;; (stream-ref exp-series 3) ; 1/6
;; (stream-ref exp-series 4) ; 1/24
;; (stream-ref exp-series 5) ; 1/120

;;    Show how to generate the series for sine and cosine,
;;    starting from the facts that the derivative of sine is cosine
;;    and the derivative of cosine is the negative of sine.

(define cosine-series
  (cons-stream 1
               (scale-stream
                (integrate-series sine-series)
                -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; (stream-ref cosine-series 0) ; 1
;; (stream-ref cosine-series 1) ; 0
;; (stream-ref cosine-series 2) ; -1/2
;; (stream-ref cosine-series 3) ; 0
;; (stream-ref cosine-series 4) ; 1/24
;; (stream-ref cosine-series 5) ; 0

;; (stream-ref sine-series 0) ; 0
;; (stream-ref sine-series 1) ; 1
;; (stream-ref sine-series 2) ; 0
;; (stream-ref sine-series 3) ; -1/6
;; (stream-ref sine-series 4) ; 0
;; (stream-ref sine-series 5) ; 1/120
