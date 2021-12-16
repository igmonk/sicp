;; Exercise 3.56
;;
;; A famous problem, first raised by R. Hamming, is to enumerate,
;; in ascending order with no repetitions, all positive integers
;; with no prime factors other than 2, 3, or 5.
;;
;; One obvious way to do this is to simply test each integer in turn
;; to see whether it has any factors other than 2, 3, and 5.
;;
;; But this is very inefficient, since, as the integers get larger,
;; fewer and fewer of them fit the requirement.
;;
;; As an alternative, let us call the required stream of numbers S
;; and notice the following facts about it.
;;
;; - S begins with 1
;; - The elements of (scale-stream S 2) are also elements of S
;; - The same is true for (scale-stream S 3) and (scale-stream S 5)
;; - These are all elements of S
;;
;; Now all we have to do is combine elements from these sources.
;;
;; For this we define a procedure 'merge' that combines two ordered
;; streams into one ordered result stream, eliminating repetitions:

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1-car (stream-car s1))
               (s2-car (stream-car s2)))
           (cond ((< s1-car s2-car)
                  (cons-stream s1-car (merge (stream-cdr s1) s2)))
                 ((> s1-car s2-car)
                  (cons-stream s2-car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1-car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;; Then the required stream may be constructed with 'merge',
;; as follows:

(load "workbook.scm")

(define S
  (cons-stream 1
               (merge (scale-stream s 2)
                      (merge (scale-stream s 3)
                             (scale-stream s 5)))))


;; Tests:
;;
;; (stream-ref S 0) ; 1
;; (stream-ref S 1) ; 2
;; (stream-ref S 2) ; 3
;; (stream-ref S 3) ; 4
;; (stream-ref S 4) ; 5
;; (stream-ref S 5) ; 6
;; (stream-ref S 6) ; 8
;; (stream-ref S 7) ; 9
;; (stream-ref S 8) ; 10
;; (stream-ref S 9) ; 12
;; (stream-ref S 10) ; 15
;; (stream-ref S 11) ; 16
;; (stream-ref S 12) ; 18
;; (stream-ref S 13) ; 20
;; (stream-ref S 14) ; 24
;; (stream-ref S 15) ; 25
;; (stream-ref S 16) ; 27
;;
;; The following numbers are not part of the resulting stream S,
;; since one of their factors if not in (2, 3, 5):
;;
;; 7x, 11x, 13x, 17x, 19x, 23x, etc.
