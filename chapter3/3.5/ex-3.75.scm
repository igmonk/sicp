;; Exercise 3.75
;;
;; Unfortunately, Alyssa's zero-crossing detector in exercise 3.74
;; proves to be insufficient, because the noisy signal from the sensor
;; leads to spurious zero crossings.
;;
;; Lem E. Tweakit, a hardware specialist, suggests that Alyssa
;; smooth the signal to filter out the noise before extracting
;; the zero crossings.
;;
;; Alyssa takes his advice and decides to extract the zero crossings
;; from the signal constructed by averaging each value of the sense data
;; with the previous value.
;;
;; She explains the problem to her assistant, Louis Reasoner,
;; who attempts to implement the idea, altering Alyssa's program
;; as follows:

(load "workbook.scm")
(load "ex-3.74.scm")

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0))

;; (display-stream zero-crossings)
;;
;; 0
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


;; This does not correctly implement Alyssa's plan.
;;
;; Find the bug that Louis has installed and fix it
;; without changing the structure of the program.
;;
;; (Hint: You will need to increase the number of arguments to
;; make-zero-crossings.)


;; The bug is introduced by mixing the input stream values
;; with the averages, which must only be used when computing
;; output:

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

;; (display-stream zero-crossings)
;;
;; 0
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
