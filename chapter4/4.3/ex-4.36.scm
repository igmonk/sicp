;; Exercise 4.36
;;
;; Exercise 3.69 discussed how to generate the stream of
;; all Pythagorean triples, with no upper bound on the size
;; of the integers to be searched.
;;
;; Explain why simply replacing an-integer-between by
;; an-integer-starting-from in the procedure in exercise 4.35
;; is not an adequate way to generate arbitrary Pythagorean triples.
;;
;; Write a procedure that actually will accomplish this.
;;
;; (That is, write a procedure for which repeatedly typing try-again
;; would in principle eventually generate all Pythagorean triples.)


;; Replacing an-integer-between by an-integer-starting-from
;; does not work because it goes on exploring all the possible
;; values of k (whose number is infinite), whereas sticking to
;; the first succeeded values of j and i - the essence of depth-first.
;;
;; Since there is no Pythagorean triple with two equal legs in rational
;; numbers, the amb evaluator never produces a value, but instead
;; perpetually invokes the failure continuation of the last
;; choice point (enumerating over all possible values of k).
;;
;; The reason for ending up in the infinite computation,
;; instead of being aborted by the max recursion depth,
;; is the iterative nature of computing the next choice -
;; a call to the failure continuation is done via tail recursion.

(define (a-pythagorean-triple-from low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; With the above implementation, the following expressions
;; cause the infinite depth-first search:
;;
;; (a-pythagorean-triple-from 1)
;; (a-pythagorean-triple-from 3)


;; As in section 3.5.3, to handle infinite range of choices,
;; we need to devise an order of combination that ensures that
;; every element will eventually be reached if we let our program
;; run long enough.
;;
;; One way of solving this is to make the amb evaluator implement
;; breadth-first search (BFS) of the graph of choices by
;; making use of conditionals:

(define (a-triple x y z)
  (amb (list x y z)
       (cond ((= x y z)
              (a-triple 1 1 (+ z 1)))
             ((= x y)
              (a-triple 1 (+ y 1) z))
             (else
              (a-triple (+ x 1) y z)))))

;; (gen-triple 1 1 1) ; (1 1 1)
;;
;; try-again ; (1 1 2)
;; try-again ; (1 2 2)
;; try-again ; (2 2 2)
;; try-again ; (1 1 3)
;; try-again ; (1 2 3)
;; try-again ; (2 2 3)
;; try-again ; (1 3 3)
;; try-again ; (2 3 3)
;; try-again ; (3 3 3)
;; try-again ; (1 1 4)
;; try-again ; (1 2 4)
;; try-again ; (2 2 4)
;; try-again ; (1 3 4)
;; try-again ; (2 3 4)
;; try-again ; (3 3 4)
;; try-again ; (1 4 4)
;; try-again ; (2 4 4)
;; try-again ; (3 4 4)
;; try-again ; (4 4 4)
;; try-again ; (1 1 5), etc.


(define (a-pythagorean-triple)
  (let ((triple (a-triple 1 1 1)))
    (let ((i (car triple))
          (j (car (cdr triple)))
          (k (car (cdr (cdr triple)))))
      (require (= (+ (* i i) (* j j)) (* k k)))
      (list i j k))))

;; (a-pythagorean-triple) ; (3 4 5)

;; try-again ; (6 8 10)
;; try-again ; (5 12 13)
;; try-again ; (9 12 15)
;; try-again ; (8 15 17)
;; try-again ; (12 16 20)
;; try-again ; (15 20 25)
;; try-again ; (7 24 25)
;; try-again ; (10 24 46)
;; try-again ; (20 21 29), etc.
