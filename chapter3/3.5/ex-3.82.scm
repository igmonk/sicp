;; Exercise 3.82
;;
;; Redo exercise 3.5 on Monte Carlo integration in terms of streams.
;;
;; The stream version of estimate-integral will not have an argument
;; telling how many trials to perform. Instead, it will produce
;; a stream of estimates based on successively more trials.

(load "../ch3support.scm")
(load "workbook.scm")

(define random-init (random 100))


;; 1. Start with a few auxiliary procedures & streams:

;; 1.1 The following procedure returns a number chosen at random
;;     from a given range:

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; (random-in-range 10 15) ; 13
;; (random-in-range 10 15) ; 14
;; (random-in-range 10 15) ; 12

;; (random-in-range -1 1) ; 0
;; (random-in-range -1 1) ; -1


;; 1.2 A stream of random points defined within the given bounds

(define (random-points x1 x2 y1 y2)
  (cons-stream
   (cons (random-in-range x1 x2)
         (random-in-range y1 y2))
   (random-points x1 x2 y1 y2)))


;; 1.3 General Circle predicate: (x-x0)^2 + (y-y0)^2 <= r^2

(define (make-circle-predicate x0 y0 r)
  (lambda (x y)
    (<= (+ (square (- x x0)) (square (- y y0)))
        (square r))))


;; 1.4 Rectangle area

(define (rect-area x1 y1 x2 y2)
  (abs (* (- x1 x2) (- y1 y2))))


;; 2. Estimating the area of the circle (x-5)^2 + (y-7)^2 < 3^2

;; 2.1 Generate a stream of points within the rectangle
;;     with diagonally opposite corners at (2,4) and (8,10) 

(define rp-stream (random-points 2 8 4 10))

;; (stream-ref rp-stream 0) ; (3 . 9)
;; (stream-ref rp-stream 1) ; (3 . 8)
;; (stream-ref rp-stream 2) ; (5 . 7)
;; (stream-ref rp-stream 3) ; (3 . 9)
;; (stream-ref rp-stream 4) ; (2 . 6)
;; (stream-ref rp-stream 5) ; (5 . 4)


;; 2.2 Generate a stream of experiments
;;     (a circle of radius 3 centered at 5,7)

(define experiment-stream
  (let ((predicate (make-circle-predicate 5 7 3)))
    (stream-map (lambda (p)
                  (predicate (car p) (cdr p)))
              rp-stream)))

;; (stream-ref experiment-stream 0) ; false
;; (stream-ref experiment-stream 1) ; true
;; (stream-ref experiment-stream 2) ; true
;; (stream-ref experiment-stream 3) ; true
;; (stream-ref experiment-stream 4) ; true
;; (stream-ref experiment-stream 5) ; true
;; (stream-ref experiment-stream 6) ; true
;; (stream-ref experiment-stream 7) ; true
;; (stream-ref experiment-stream 8) ; false


;; 2.3 Rectangle area

(define rect-area-2-4-8-10 (rect-area 2 4 8 10))


;; 2.4 Estimating the circle area

(define circle-573-area
  (stream-map (lambda (p) (* p rect-area-2-4-8-10))
              (monte-carlo experiment-stream 0 0)))

;; (stream-ref circle-573-area 0) ; 0
;; (stream-ref circle-573-area 1) ; 18
;; (stream-ref circle-573-area 2) ; 24
;; (stream-ref circle-573-area 3) ; 27
;; (stream-ref circle-573-area 4) ; 144/5
;; (stream-ref circle-573-area 5) ; 30

;; (stream-ref circle-573-area 105) ; 1314/53
;; (stream-ref circle-573-area 106) ; 2664/107
;; (stream-ref circle-573-area 107) ; 25

;; (stream-ref circle-573-area 1005) ; 13788/503


;; 3 Estimating pi

;; 3.1 Generate a stream of points within the rectangle
;;     with diagonally opposite corners at (-1,-1) and (1,1) 

(define rp-unit-stream (random-points -1 1 -1 1))

;; (stream-ref rp-unit-stream 0) ; (-1 . -1)
;; (stream-ref rp-unit-stream 1) ; (-1 . -1)
;; (stream-ref rp-unit-stream 2) ; (0 . 0)
;; (stream-ref rp-unit-stream 3) ; (0 . -1)
;; (stream-ref rp-unit-stream 4) ; (-1 . 0)
;; (stream-ref rp-unit-stream 5) ; (0 . 0)


;; 3.2 Unit Circle predicate:  x^2 + y^2 <= 1

(define unit-circle-predicate
  (make-circle-predicate 0 0 1))


;; 3.3 Generate a stream of experiments (unit circle)

(define unit-experiment-stream
  (stream-map (lambda (p)
                (unit-circle-predicate (car p) (cdr p)))
              rp-unit-stream))

;; (stream-ref unit-experiment-stream 0) ; false
;; (stream-ref unit-experiment-stream 1) ; false
;; (stream-ref unit-experiment-stream 2) ; true
;; (stream-ref unit-experiment-stream 3) ; true
;; (stream-ref unit-experiment-stream 4) ; true
;; (stream-ref unit-experiment-stream 5) ; true
;; (stream-ref unit-experiment-stream 6) ; true
;; (stream-ref unit-experiment-stream 7) ; false


;; 3.4 Rectangle area

(define rect-area-unit (rect-area -1 -1 1 1))


;; 3.5 Estimating pi (= the area of the unit circle)

(define pi
  (stream-map (lambda (p) (* p rect-area-unit))
              (monte-carlo unit-experiment-stream 0 0)))

;; (stream-ref pi 0) ; 0
;; (stream-ref pi 1) ; 0
;; (stream-ref pi 2) ; 4/3
;; (stream-ref pi 3) ; 2
;; (stream-ref pi 4) ; 12/5
;; (stream-ref pi 5) ; 8/3

;; (stream-ref pi 105) ; 160/53 = 3,0188679245
;; (stream-ref pi 106) ; 342/107
;; (stream-ref pi 107) ; 82/27

;; (stream-ref pi 1005) ; 1514/503 = 3,0099403579
