;; Exercise 4.50
;;
;; Implement a new special form 'ramb' that is like 'amb'
;; except that it searches alternatives in a random order,
;; rather than from left to right.
;;
;; Show how this can help with Alyssa's problem in exercise 4.49.


;; The syntax analysis for ramb is almost the same as that
;; for amb, except the place where it is decided which item
;; on the list of choices is to be tried next:
;; take one randomly and use the rest to proceed when the
;; failure continuation is called.

(define (analyze-ramb exp)
  (let ((cprocs (map _analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((rand-index (random (length choices))))
              (let ((first (list-ref choices rand-index))
                    (rest (list-except choices rand-index)))
                (first env
                       succeed
                       (lambda ()
                         (try-next rest)))))))
      (try-next cprocs))))

;; The procedure random is used to define
;; - the index of an element to be chosen next
;; - the remaining choices

(define (list-except l k)
  (cond ((null? l) l)
        ((>= k (length l))
         (error "The index goes beyond the list -- LIST-EXCEPT"
                (list l k)))
        (else (append (list-head l k)
                      (list-tail l (+ k 1))))))

;; See: eval-ramb.scm
;;      list-utils.scm


;; Tests
;;
;; (ramb 1 2 3 4 5) ; 1
;; try-again        ; 3
;; try-again        ; 2
;; try-again        ; 5
;; try-again        ; 4
;; try-again        ; There are no more values
;;
;; (ramb 1 2 3 4 5) ; 5
;; try-again        ; 3
;; try-again        ; 1
;; try-again        ; 2
;; try-again        ; 4
;; try-again        ; There are no more values


;; Below is show how the newly created form can help
;; with Alyssa's problem in exercise 4.49.

(define (parse-word word-list)
  (define (inner part-of-speech words)
    (require (not (null? words)))
    (ramb (list part-of-speech (car words))
          (inner part-of-speech (cdr words))))
  (inner (car word-list) (cdr word-list)))

;; Run in the driver loop (with all the necessary definitions).

(parse '()) ; the student sleeps
(parse '()) ; a student lectures
(parse '()) ; a professor studies

(parse '()) ; the professor sleeps
try-again   ; the professor sleeps in a student
try-again   ; the professor sleeps in a student for the class
