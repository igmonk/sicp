;; Exercise 3.55
;;
;; Define a procedure 'partial-sums' that takes as argument a stream S
;; and returns the stream whose elements are
;;
;; S0, S0 + S1, S0 + S1 + S2, ...
;;
;; For example, (partial-sums integers) should be the stream
;;
;; 1, 3, 6, 10, 15, ....


;; The element at index i of the resulting stream 'res' is computed by:
;; 
;; res(i) = s[0][i] + s[1][i-1] + s[2][i-2] + ... + s[i-1][1] + s[i][0]
;;
;; where  s[0] is the original stream
;;        s[1] is (stream-cdr s[0])
;;        s[2] is (stream-cdr s[1]), etc.
;;
;;  res: x0 x1 x2 x3 ...
;;        ↑  ↑  ↑  ↑
;; s[0]: s0 s1 s2 s3 s4 s5 ...
;; s[1]:    s0 s1 s2 s3 s4 s5 ...
;; s[2]:       s0 s1 s2 s3 s4 s5 ...
;; s[3]:          s0 s1 s2 s3 s4 s5 ...
;; ...               ...
;;
;; In other words,
;; computing each subsequent element of the resulting stream
;; forces one more element of the streams generated so far, and
;; the value of the element is the sum of the first forced items
;; of the original stream.

(load "workbook.scm")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;; (define ps-integers (partial-sums integers))
;;
;; (stream-ref ps-integers 0) ; 1
;; (stream-ref ps-integers 1) ; 3
;; (stream-ref ps-integers 2) ; 6
;; (stream-ref ps-integers 3) ; 10
;; (stream-ref ps-integers 4) ; 15
;; (stream-ref ps-integers 5) ; 21
