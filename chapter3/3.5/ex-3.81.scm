;; Exercise 3.81
;;
;; Exercise 3.6 discussed generalizing the random-number generator
;; to allow one to reset the random-number sequence so as to produce
;; repeatable sequences of "random" numbers.
;;
;; Produce a stream formulation of this same generator that operates
;; on an input stream of requests to generate a new random number or
;; to reset the sequence to a specified value and that produces
;; the desired stream of random numbers.
;;
;; Don't use assignment in your solution.

(load "../ch3support.scm")

(define random-init (random 100))

(define requests
  (stream '(generate)
          '(generate)
          '(generate)
          '(reset 25)
          '(generate)
          '(generate)
          '(generate)
          '(reset 25)
          '(generate)
          '(generate)
          '(generate)
          '(reset 105)
          '(generate)
          '(generate)
          '(generate)))

(define random-numbers
  (cons-stream random-init
               (stream-map custom-update
                           random-numbers
                           requests)))

(define (custom-update x req)
  (cond ((equal? req '(generate))
         (rand-update x))
        ((and (pair? req) (eq? (car req) 'reset))
         (cadr req))
        (else
         (error "Wrong request -- CUSTOM-UPDATE " req))))


;; (stream-ref random-numbers 0) ; 52
;; (stream-ref random-numbers 1) ; 33
;; (stream-ref random-numbers 2) ; 28

;; (stream-ref random-numbers 3) ; 20 [returns x and resets it to 25]

;; (stream-ref random-numbers 4) ; 25
;; (stream-ref random-numbers 5) ; 66
;; (stream-ref random-numbers 6) ; 30

;; (stream-ref random-numbers 7) ; 74 [returns x and resets it to 25]

;; (stream-ref random-numbers 8)  ; 25
;; (stream-ref random-numbers 9)  ; 66
;; (stream-ref random-numbers 10) ; 30

;; (stream-ref random-numbers 11) ; 74 [returns x and resets it to 105]

;; (stream-ref random-numbers 12) ; 105
;; (stream-ref random-numbers 13) ; 67
;; (stream-ref random-numbers 14) ; 57
