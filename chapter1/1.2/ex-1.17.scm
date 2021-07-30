;; Exercise 1.17
;;
;; The exponentiation algorithms in this section are based on performing exponentiation
;; by means of repeated multiplication.
;; In a similar way, one can perform integer multiplication by means of repeated addition.
;;
;; The following multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;; (* 0 0)   ; 0
;; (* 0 1)   ; 0
;; (* 1 1)   ; 1
;; (* 1 10)  ; 10
;; (* 10 10) ; 100
;; (* 5 25)  ; 125

;; This algorithm takes a number of steps that is linear in b.
;;
;; Now suppose we include, together with addition, operations double, which doubles an integer,
;; and halve, which divides an (even) integer by 2.
;; Using these, design a multiplication procedure analogous to fast-expt that uses
;; a logarithmic number of steps.

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast* a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast* a (halve b)))) ;; a * b = 2(a * b/2)
	(else (+ a (fast* a (- b 1))))))

;; (fast* 0 0)   ; 0
;; (fast* 0 1)   ; 0
;; (fast* 1 1)   ; 1
;; (fast* 1 10)  ; 10
;; (fast* 10 10) ; 100
;; (fast* 5 25)  ; 125
