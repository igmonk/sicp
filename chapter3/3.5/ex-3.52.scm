;; Exercise 3.52
;;
;; Consider the sequence of expressions

(load "workbook.scm")
(load "ex-3.50.scm")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

;; sum ; 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

;; sum ; 1
;; seq ; {1 ...}
;;
;; seq represents historical values of sum as a stream.
;;
;; The definition itself forces the 1st item of the stream that
;; enumerates the interval, which leads to a single invocation
;; of the accum procedure, setting sum to 1, which becomes
;; the 1st item of seq.

(define y (stream-filter even? seq))

;; sum ; 6
;; seq ; {1 3 6 ...}
;; y   ; {6 ...}
;;
;; y represents even values of seq as a stream.
;;
;; The definition forces the first 3 values of seq, which
;; is enough to initialize the 1st item of the stream y.

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;;
;; sum ; 10
;; seq ; {1 3 6 10 ...}
;; y   ; {6 ...}
;; z   ; {10 ...}
;;
;; z represents those values of seq that are divisible by 5,
;; as a stream.
;;
;; The definition forces one more element of seq - 4th -
;; until the value divisible by 5 is found.

;; (stream-ref y 7) ; 136
;;
;; sum ; 136
;; seq ; {1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 ...}
;; y   ; {6 10 28 36 66 78 120 136 ...}
;; z   ; {10 ...}
;;
;; The invocation forces some more elements of y,
;; which, in turn, forces some more elements of seq
;; and updates the value of sum, accordingly.


;; (display-stream z)
;;
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; Value: done
;;
;; sum ; 210
;; seq ; {1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210}
;; y   ; {6 10 28 36 66 78 120 136 ...}
;; z   ; {10 15 45 55 105 120 190 210}
;;
;; display-stream invokes stream-for-each, which forces the entire stream.


;; What is the value of sum after each of the above expressions
;; is evaluated?
;;
;; What is the printed response to evaluating the stream-ref and
;; display-stream expressions?

;; Explained above.


;; Would these responses differ if we had implemented
;;
;; (delay <exp>)
;;
;; simply as
;;
;; (lambda () <exp>)
;;
;; without using the optimization provided by memo-proc? Explain.

;; Yes, they would. Without memo-proc? we would end up forcing
;; the same delayed object many times.
;;
;; In the example above, forcing seq involves calling
;; the procedure accum that has a side effect - it updates
;; the value of sum. Therefore, repetitive forcings of seq
;; from different clients of this stream would cause sum
;; to change without necessity.
