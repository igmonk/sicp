;; Exercise 3.76
;;
;; Eva Lu Ator has a criticism of Louis's approach in exercise 3.75.
;;
;; The program he wrote is not modular, because it intermixes
;; the operation of smoothing with the zero-crossing extraction.
;;
;; For example, the extractor should not have to be changed if Alyssa
;; finds a better way to condition her input signal.
;;
;; Help Louis by writing a procedure smooth that takes a stream as input
;; and produces a stream in which each element is the average of
;; two successive input stream elements.
;;
;; Then use smooth as a component to implement the zero-crossing detector
;; in a more modular style.

(load "../../common.scm")
(load "workbook.scm")
(load "ex-3.74.scm")

(define (smooth s)
  (stream-map average (cons-stream 0 s) s))

(define (make-zero-crossings input-stream smoother)
  (let ((smoothed (smoother input-stream)))
    (stream-map sign-change-detector smoothed (cons-stream 0 smoothed))))

(define zero-crossings (make-zero-crossings sense-data smooth))

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
