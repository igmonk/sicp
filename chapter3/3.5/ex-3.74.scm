;; Exercise 3.74
;;
;; Alyssa P. Hacker is designing a system to process signals
;; coming from physical sensors.
;;
;; One important feature she wishes to produce is a signal that
;; describes the zero crossings of the input signal.
;;
;; That is, the resulting signal should be
;;   + 1 whenever the input signal changes from negative to positive,
;;   - 1 whenever the input signal changes from positive to negative,
;;   and 0 otherwise.
;;
;; (Assume that the sign of a 0 input is positive.)
;;
;; For example, a typical input signal with its associated zero-crossing signal
;; would be
;;
;; ... 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4 ...
;;  ... 0  0    0  0    0     -1  0   0   0     0    1  0  0 ...

(define sense-data
  (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -5 0.2 3 4))

;; (display-stream sense-data)
;;
;; 1
;; 2
;; 1.5
;; 1
;; .5
;; -.1
;; -2
;; -3
;; -2
;; -5
;; .2
;; 3
;; 4

;; In Alyssa's system, the signal from the sensor is represented as
;; a stream sense-data and the stream zero-crossings is the corresponding
;; stream of zero crossings.

(define (sign-change-detector x1 x0)
  (cond ((and (< x0 0) (>= x1 0)) 1)
        ((and (>= x0 0) (< x1 0)) -1)
        (else 0)))

;; (sign-change-detector -5 5)  ; 1
;; (sign-change-detector 5 -5)  ; -1
;; (sign-change-detector 5 5)   ; 0
;; (sign-change-detector -5 -5) ; 0

;; Alyssa first writes a procedure sign-change-detector that takes two values
;; as arguments and compares the signs of the values to produce an appropriate
;; 0, 1, or - 1.
;;
;; She then constructs her zero-crossing stream as follows:

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

;; (display-stream zero-crossings)
;;
;; 0
;; 0
;; 0
;; 0
;; 0
;; -1
;; 0
;; 0
;; 0
;; 0
;; 1
;; 0
;; 0
;;
;; At the end the following error is thrown
;;   The object (), passed as the first argument to car, is not the correct type
;;
;; due to the stream procedure make-zero-crossings expects an infinite stream
;; as input, but, when given a finite one, produces a null element eventually.
;; (make-zero-crossings does not perform any stream-null? checks.)


;; Alyssa's boss, Eva Lu Ator, walks by and suggests that this program
;; is approximately equivalent to the following one, which uses
;; the generalized version of stream-map from exercise 3.50:
;;
;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data <expression>))
;;
;; Complete the program by supplying the indicated <expression>.

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; (display-stream zero-crossings)
;;
;; 0
;; 0
;; 0
;; 0
;; 0
;; -1
;; 0
;; 0
;; 0
;; 0
;; 1
;; 0
;; 0
;; ;Value: done
;;
;; No exception is thrown at the end, since stream-map performs
;; stream-null? unlike the procedure make-zero-crossings.
