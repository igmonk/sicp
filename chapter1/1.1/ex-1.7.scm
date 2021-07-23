;; Exercise 1.7
;;
;; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always performed with limited precision.
;; This makes our test inadequate for very large numbers.
;; Explain these statements, with examples showing how the test fails for small and large numbers.
;; An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

(define (average x y)
  (/ (+ x y) 2))

;; (average 1 3)  ; 2
;; (average -1 1) ; 0

;; A guess is improved by averaging it with the quotient of the radicand and the old guess.
(define (improve guess x)
  (average guess (/ x guess)))

;; (improve 1 2) ; 3/2

;; The idea is to improve the answer until it is close enough so that its square
;; differs from the radicand by less than a predetermined tolerance (here 0.001).
(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.001))

;; (good-enough? 10 100)      ; true
;; (good-enough? 9.9 100)     ; false
;; (good-enough? 9.99999 100) ; true

;; We start with a value for the radicand and a value for the guess.
;; If the guess is good enough for the purpose, we are done;
;; if not, we must repeat the process with an improved guess.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; (sqrt 9)                         ; 3.00009155413138
;; (sqrt (+ (square 3) (square 4))) ; 5.000023178253949
;; (sqrt 0.01)                      ; .10032578510960605
;; (sqrt 0.0001)                    ; .10032578510960605


;; 1. Very small numbers
;;
;; Computing square roots for very small numbers is not effective enough:
;;
;; (sqrt 0.000001)   ; 3.1260655525445276e-2 ; Expected: 0.001
;; (sqrt 0.00000001) ; .03125010656242753    ; Expected: 0,0001
;;
;; That happens due to the accuracy of the predetermined tolerance of 0.001 is comparably lower than that of the change in each subsequent guess (or squared guess).
;; Therefore, the specified guess requirement is met before an adequate solution is approached.
;;
;;
;; 2. Very large numbers
;; 
;; (sqrt 123456789123456789123456789123456789) ; Takes forever to evaluate
;;
;; By printing out the current guess value, we can see it get stuck at one point:
;;
;; ...
;; 5.4826578530344395e19
;; 2.7414415149665018e19
;; 1.3709459251340095e19
;; 6.859232239043052e18
;; 3.438615434956622e18
;; 1.737259243529815e18
;; 904161678820656400.
;; 520352251490348900.
;; 378804217359904960.
;; 352358041409391300.
;; 351365584674747140.
;; 351364183042923900.
;; 351364183040128260. <- never changes after this point
;; 351364183040128260.
;; 351364183040128260.
;; 351364183040128260.
;; 351364183040128260.
;; 351364183040128260.
;; ... and so on
;;
;; What we observe here is floating point arithmetic that produces inexact numbers:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Exactness.html
;;
;; MIT/GNU Scheme follows the IEEE 754-2008 floating-point standard, using binary64 arithmetic for flonums:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Flonum-Operations.html
;;
;; The IEEE 754-2008 standard defines five basic formats:
;; - three binary formats, with encodings in lengths of 32, 64, and 128 bits
;; - two decimal formats, with encodings in lengths of 64 and 128 bits
;; 
;; https://ieeexplore.ieee.org/document/4610935
;; http://www.dsc.ufcg.edu.br/~cnum/modulos/Modulo2/IEEE754_2008.pdf
;;
;; Thus, at some point when a guess is squared, the precision of the underlying inexact number is reduced:
;; its significand - the part of a number in scientific notation or in floating-point representation, consisting of its significant digit - requires more space to be fully represented.
;;
;; That prevents the difference between the squared guess and radicant from overcoming the predetermined tolerance of 0.001. As a result, the algorithm enters an infinite recursion.
;;
;;
;; An alternative strategy for implementing good-enough?

;; The idea is to watch how guess changes from one iteration to the next and
;; to stop when the change is a very small fraction of the guess
(define (new-good-enough? prev-guess guess)
  (< (abs (- guess prev-guess)) 0.001))

;; (new-good-enough? 0 0.01)   ; false
;; (new-good-enough? 0 0.001)  ; false
;; (new-good-enough? 0 0.0001) ; true

;; We start with a value for the radicand and a value for the previous and current guesses.
;; If the current guess is good enough for the purpose, we are done;
;; if not, we must repeat the process with an improved guess.
(define (new-sqrt-iter prev-guess guess x)
  (if (new-good-enough? prev-guess guess)
      guess
      (new-sqrt-iter guess (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrt-iter 100.0 1.0 x))

;; (new-sqrt 100)  ; 10.00000000013989
;; (new-sqrt 0.01) ; .1000005289564269
;;
;; Very small numbers:
;;
;; (new-sqrt 0.000001) ; 1.2961915927068783e-3
;; (new-sqrt 0.00000001) ; 9.799734463768973e-4
;;
;; Very large numbers:
;;
;; (new-sqrt 123456789123456789123456789123456789) ; 351364183040128260.
;;
;; Conclusion:
;;
;; The alternative solution that is based on observing of how guess changes
;; allows for rather precise square root evaluation of very small numbers
;; as well as for more sustainable square root evaluation of very large numbers -
;; the algorithm avoids infinite recursion (although, its result could be not ideally precise). 
