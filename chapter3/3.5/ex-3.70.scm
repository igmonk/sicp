;; Exercise 3.70
;;
;; It would be nice to be able to generate streams in which
;; the pairs appear in some useful order, rather than in the order
;; that results from an ad hoc interleaving process.
;;
;; We can use a technique similar to the merge procedure
;; of exercise 3.56, if we define a way to say that
;; one pair of integers is "less than" another.
;;
;; One way to do this is to define a "weighting function" W(i,j)
;; and stipulate that (i1,j1) is less than (i2,j2)
;; if W(i1,j1) < W(i2,j2).
;;
;; Write a procedure merge-weighted that is like merge,
;; except that merge-weighted takes an additional argument weight,
;; which is a procedure that computes the weight of a pair,
;; and is used to determine the order in which elements should appear
;; in the resulting merged stream.
;;
;; Using this, generalize pairs to a procedure weighted-pairs that
;; takes two streams, together with a procedure that computes
;; a weighting function, and generates the stream of pairs,
;; ordered according to weight.
;;
;; Use your procedure to generate
;; a. the stream of all pairs of positive integers (i,j)
;;    with i <= j ordered according to the sum i + j
;; b. the stream of all pairs of positive integers (i,j)
;;    with i <= j, where neither i nor j is divisible by 2, 3, or 5,
;;    and the pairs are ordered according to the sum 2i + 3j + 5ij.


(load "../../common.scm")
(load "workbook.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1-car (stream-car s1))
               (s2-car (stream-car s2)))
           (cond ((<= (weight s1-car) (weight s2-car))
                  (cons-stream s1-car
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2-car
                               (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))


;; a. the stream of all pairs of positive integers (i,j)
;;    with i <= j ordered according to the sum i + j

(define (weight-a pair) (apply + pair))

(define stream-a
  (weighted-pairs integers integers weight-a))

;; (stream-ref stream-a 0) ; (1 1)
;; (stream-ref stream-a 1) ; (1 2)
;; (stream-ref stream-a 2) ; (1 3)
;; (stream-ref stream-a 3) ; (2 2)
;; (stream-ref stream-a 4) ; (1 4)
;; (stream-ref stream-a 5) ; (2 3)
;; (stream-ref stream-a 6) ; (1 5)
;; (stream-ref stream-a 7) ; (2 4)
;; (stream-ref stream-a 8) ; (3 3)
;; (stream-ref stream-a 9) ; (1 6)


;; b. the stream of all pairs of positive integers (i,j)
;;    with i <= j, where neither i nor j is divisible by 2, 3, or 5,
;;    and the pairs are ordered according to the sum 2i + 3j + 5ij.

(define (weight-b pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (divisible-by-2-3-or-5? x)
  (or (divisible? x 2)
      (divisible? x 3)
      (divisible? x 5)))

;; (divisible-by-2-3-or-5? 1) ; false
;; (divisible-by-2-3-or-5? 2) ; true
;; (divisible-by-2-3-or-5? 3) ; true
;; (divisible-by-2-3-or-5? 4) ; true
;; (divisible-by-2-3-or-5? 5) ; true
;; (divisible-by-2-3-or-5? 6) ; true
;; (divisible-by-2-3-or-5? 7) ; false
;; (divisible-by-2-3-or-5? 8) ; true
;; (divisible-by-2-3-or-5? 9) ; true


(define (predicate-b pair)
  (and (not (divisible-by-2-3-or-5? (car pair)))
       (not (divisible-by-2-3-or-5? (cadr pair)))))

(define stream-b
  (stream-filter predicate-b
                 (weighted-pairs integers integers weight-b)))

;; (stream-ref stream-b 0)  ; (1 1)
;; (stream-ref stream-b 1)  ; (1 7)
;; (stream-ref stream-b 2)  ; (1 11)
;; (stream-ref stream-b 3)  ; (1 13)
;; (stream-ref stream-b 4)  ; (1 17)
;; (stream-ref stream-b 5)  ; (1 19)
;; (stream-ref stream-b 6)  ; (1 23)
;; (stream-ref stream-b 7)  ; (1 29)
;; (stream-ref stream-b 8)  ; (1 31)
;; (stream-ref stream-b 9)  ; (7 7)
;; (stream-ref stream-b 10) ; (1 37)
;; (stream-ref stream-b 11) ; (1 41)
;; (stream-ref stream-b 12) ; (1 43)
;; (stream-ref stream-b 13) ; (1 47)
;; (stream-ref stream-b 14) ; (1 49)
;; (stream-ref stream-b 15) ; (1 53)
;; (stream-ref stream-b 16) ; (7 11)
;; (stream-ref stream-b 17) ; (1 59)


;; As option, the stream integers might have been filtered
;; in advance:
;;
;; (define integers-filtered-235
;;   (stream-filter
;;    (lambda (x) (not (divisible-by-2-3-or-5? x)))
;;    integers))
;;
;; (stream-ref integers-filtered-235 0) ; 1
;; (stream-ref integers-filtered-235 1) ; 7
;; (stream-ref integers-filtered-235 2) ; 11
;; (stream-ref integers-filtered-235 3) ; 13
;; (stream-ref integers-filtered-235 4) ; 17
;; (stream-ref integers-filtered-235 5) ; 19
;;
;; so that the stream b would have been defined as follows:
;;
;; (define stream-b
;;   (weighted-pairs integers-filtered-235
;;                   integers-filtered-235
;;                   weight-b))
