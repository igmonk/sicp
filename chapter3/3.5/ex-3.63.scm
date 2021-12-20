;; Exercise 3.63
;;
;; Louis Reasoner asks why the sqrt-stream procedure was not written
;; in the following more straightforward way, without
;; the local variable guesses:

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;; Alyssa P. Hacker replies that this version of the procedure is
;; considerably less efficient because it performs redundant computation.
;;
;; Explain Alyssa's answer.
;;
;; Would the two versions still differ in efficiency if
;; our implementation of delay used only (lambda () <exp>)
;; without using the optimization provided by memo-proc (section 3.5.1)?


;; This version of the procedure is considerably less efficient,
;; since each time a subsequent element of the stream is forced,
;; a new stream is generated (by calling sqrt-stream),
;; where all the values need to be recomputed in contrast to
;; the original procedure sqrt-stream that reuses the inner stream,
;; the values of which, once computed, are memoized.
;;
;; (can be observed by analyzing the invocations of the inner
;; lambda function used as a mapper function)
;;
;; The two versions would have be equal in efficiency,
;; had the implementation os delay used only (lambda () <exp>)
;; withouth the optimization provided by memo-proc.
