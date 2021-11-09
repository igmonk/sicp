;; Exercise 3.6
;;
;; It is useful to be able to reset a random-number generator to produce
;; a sequence starting from a given value.
;;
;; Design a new rand procedure that is called with an argument that is
;; either the symbol 'generate' or the symbol 'reset' and behaves as follows:
;;   (rand 'generate) produces a new random number;
;;   ((rand 'reset) <new-value>) resets the internal state variable to
;;                               the designated <new-value>.
;;
;; Thus, by resetting the state, one can generate repeatable sequences.
;; These are very handy to have when testing and debugging programs that
;; use random numbers.

(load "../ch3support.scm")

(define random-init (random 100))

(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (set! x (rand-update x))
             x)
            ((eq? op 'reset)
             (lambda (new-value)
               (set! x new-value)
               x))
            (else
             (error "Unsupported operation -- RAND" op))))))


;; Tests

;; (rand 'generate) ; 31
;; (rand 'generate) ; 101
;; (rand 'generate) ; 86

;; ((rand 'reset) 25) ; 25

;; (rand 'generate) ; 66
;; (rand 'generate) ; 30
;; (rand 'generate) ; 74

;; ((rand 'reset) 25) ; 25

;; (rand 'generate) ; 66
;; (rand 'generate) ; 30
;; (rand 'generate) ; 74

;; ((rand 'reset) 105) ; 105

;; (rand 'generate) ; 67
;; (rand 'generate) ; 57
;; (rand 'generate) ; 41

;; (rand 'unknown) ; Unsupported operation -- RAND unknown
