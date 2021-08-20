;; Exercise 1.37
;;
;; a. An infinite continued fraction is an expression of the form
;;
;; f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...)))
;;
;; As an example, one can show that the infinite continued fraction expansion with
;; the Ni and the Di all equal to 1 produces 1/φ, where φ is the golden ratio.
;;
;; One way to approximate an infinite continued fraction is to truncate the expansion
;; after a given number of terms.
;; Such a truncation - a so-called k-term finite continued fraction - has the form
;;
;; N1 / (D1 + N2 / (... + Nk/Dk))
;;
;; Suppose that n and d are procedures of one argument (the term index i)
;; that return the Ni and Di of the terms of the continued fraction.
;;
;; Define a procedure cont-frac such that evaluating (cont-frac n d k) computes
;; the value of the k-term finite continued fraction.
;;
;; b. If your cont-frac procedure generates a recursive process,
;; write one that generates an iterative process.
;; If it generates an iterative process, write one that generates a recursive process.


;; a. recursive process

(define (cont-frac n d k)
  (define (inner i)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (< i k)
	  (/ n-term (+ d-term (inner (+ i 1))))
	  (/ n-term d-term))))
  (inner 1))

;; b. iterative process

(define (cont-frac n d k)
  (define (iter i result)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (> i 0)
	  (iter (- i 1) (/ n-term (+ d-term result)))
	  result)))
  (iter k 0))

;; Check your procedure by approximating 1/φ using
;;
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)
;;
;; for successive values of k.

(define (cont-frac-k k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

;; (cont-frac-k 1)    ; 1.
;; (cont-frac-k 2)    ; .5
;; (cont-frac-k 3)    ; .6666666666666666
;; (cont-frac-k 10)   ; .6179775280898876
;; (cont-frac-k 11)   ; .6180555555555556
;; (cont-frac-k 100)  ; .6180339887498948
;; (cont-frac-k 1000) ; .6180339887498948

;; How large must you make k in order to get an approximation that is accurate to 4 decimal places?

(define (good-enough-k)
  (define (try k v)
    (let ((k-next (+ k 1)))
      (let ((v-next (cont-frac-k k-next)))
	(if (close-enough? v v-next)
	    k-next
	    (try k-next v-next)))))
  (try 1 1.0))

(define (close-enough? a b)
  (< (abs (- a b)) 0.0001))

;; (good-enough-k) ; 11
;;
;; Thus, in order to get an approximation that is accurate to 4 decimal places,
;; k must be greater or equal to 11.

;; Note:
;;
;; It is important to keep track of the exact index (i) that is used to calculate
;; the term values (Ni and Di).
;; Given the approximation 1/φ, the importance of doing that is not obvious, since
;; the return value is irrespective of the given index and always equals to 1.
;;
;; It only becomes noticeable when the term value is a dependency of a given index,
;; as shown in exercise 1.38, where Di values are not all the same.
;;
;; Hence, this observation should be taken into account when implementing cont-frac procedures
;; that generate recursive and iterative processes.
;;
;; In case a recursive process is generated, its evaluation plan unfolds from top to bottom, i.e.
;; from the nominator down to the denominator of a quotient, from lower indices to higher ones.
;; On its way back, evaluation starts with the highest index (i = k) and gradually folds up to
;; the smallest one (i = 1).
;;
;; In case an iterative process is generated, both its evaluation plan and evaluation itself
;; start with the highest index (i = k), keeping track of intermediate results, each of which is
;; a quotient calculated based on the current nominator and denominator values and the result
;; from the previous iteration.
