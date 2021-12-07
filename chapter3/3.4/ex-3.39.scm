;; Exercise 3.39
;;
;; Which of the five possibilities in the parallel execution
;; shown above remain if we instead serialize execution as follows:

(define x 10)
(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))


;; The serializer has been applied to P1 in such a way that
;; it prevents any changes to x while P1 accesses its value
;; twice to compute the square of x.
;; It does not, however, prevent an interleaved read/write
;; operations performed by another processes in between
;; P1 has computed the square but has not set x yet.
;;
;; The same serializer has also been applied to P2 so that
;; it prevents read/write operations performed by
;; other processes during the whole operation of P2.
;;
;; Hence, the following possibilities remain:
;;
;; 101: P1 sets x to 100 and then P2 increments x to 101
;; 121: P2 increments x to 11 and then P1 sets x to x times x
;; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x
