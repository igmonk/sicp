;; Exercise 1.45
;;
;; We saw in section 1.3.3 that attempting to compute square roots by naively finding
;; a fixed point of y -> x/y does not converge, and that this can be fixed by average damping.
;; The same method works for finding cube roots as fixed points of the average-damped y -> x/y^2.
;;
;; Unfortunately, the process does not work for fourth roots - a single average damp
;; is not enough to make a fixed-point search for y -> x/y^3 converge.
;;
;; On the other hand, if we average damp twice (i.e., use the average damp
;; of the average damp of y -> x/y^3) the fixed-point search does converge.
;;
;; Do some experiments to determine how many average damps are required to compute
;; nth roots as a fixed-point search based upon repeated average damping of y -> x/y^n-1.
;;
;; Use this to implement a simple procedure for computing nth roots using fixed-point,
;; average-damp, and the repeated procedure of exercise 1.43.
;; Assume that any arithmetic operations you need are available as primitives.

(load "../1.2/workbook.scm") ; Exponentiation: successive squaring
(load "workbook.scm")
(load "ex-1.43.scm")

;; By having parameterised the number of average damps
;;
;; (define (nth-root x n d)
;;   (define (improve-guess y)
;;     (/ x (expt-ss y (- n 1))))
;;   (fixed-point ((repeated average-damp d) improve-guess)
;; 	          1.0))
;;
;; it could be established how many damps are required to compute nth roots.
;;
;; Number of damps (d) required to compute nth roots:
;;
;; |  n  |  d  |
;; |-----|-----|
;; |   2 |   1 |
;; |   3 |   1 | (nth-root 8 3 1)
;; |   4 |   2 | (nth-root 16 4 2)
;; |   5 |   2 |
;; |   6 |   2 |
;; |   7 |   2 |
;; |   8 |   3 | (nth-root 256 8 3)
;; |   9 |   3 |
;; |  10 |   3 |
;; |  11 |   3 |
;; |  12 |   3 |
;; |  13 |   3 |
;; |  14 |   3 |
;; |  15 |   3 | (nth-root 32678 15 3)
;; |  16 |   4 | (nth-root 65536 16 4)
;; | ... | ... |
;;
;; From the table above, the following could be postulated:
;; each time the number of damps (d) increases by 1 it starts to solve
;; the problem of computing the 2^d-th root of a number n.
;;
;; Hence, the number of damps (d) required to compute nth roots
;; is equal to floor(log2(n)).

(define (num-of-damps n)
  (floor (log2 n)))

;; log2 can be reformulated via the natural logarithm using the logarithm base change rule:
;; log2(n) = log(n) / log(2)

(define (log2 n)
  (/ (log n) (log 2)))

;; With these definitions in place, we can now implemenent an nth-root procedure that,
;; based on a given root number, calculates the minimum required number of average damps
;; and computes the root value.

(define (nth-root x n)
  (define (improve-guess y)
    (/ x (expt-ss y (- n 1))))
  (fixed-point ((repeated average-damp (num-of-damps n)) improve-guess)
	       1.0))

;; (nth-root 2 1)      ; 1.9999923706054687
;; (nth-root 4 2)      ; 2.000000000000002
;; (nth-root 8 3)      ; 1.9999981824788517
;; (nth-root 16 4)     ; 2.0000000000021965
;; (nth-root 32 5)     ; 2.000001512995761
;; (nth-root 64 6)     ; 2.0000029334662086
;; (nth-root 128 7)    ; 2.0000035538623377
;; (nth-root 256 8)    ; 2.000000000003967
;; (nth-root 512 9)    ; 2.9999997106840102
;; (nth-root 1024 10)  ; 2.0000011830103324
;; (nth-root 2048 11)  ; 1.9999976006547362
;; (nth-root 4096 12)  ; 1.999997691470309
;; (nth-root 8192 13)  ; 2.0000029085658984
;; (nth-root 16384 14) ; 1.9999963265447058
;; (nth-root 32768 15) ; 2.0000040951543023
;; (nth-root 65536 16) ; 2.000000000076957

;; (nth-root 3 1)    ; 2.9999923706054687
;; (nth-root 9 2)    ; 3.
;; (nth-root 27 3)   ; 2.9999972321057697
;; (nth-root 81 4)   ; 3.000000000000033
;; (nth-root 243 5)  ; 3.0000008877496294
;; (nth-root 729 6)  ; 2.999996785898161
;; (nth-root 2187 7) ; 3.0000041735235943

;; (nth-root 1000 3)             ; 10.000002544054729
;; (nth-root (expt-ss 10 10) 10) ; 10.000001863218898
