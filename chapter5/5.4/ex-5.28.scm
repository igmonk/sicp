;; Exercise 5.28
;;
;; Modify the definition of the evaluator by changing eval-sequence
;; as described in section 5.4.2 so that the evaluator is no longer
;; tail-recursive.
;;
;; Rerun your experiments from exercises 5.26 and 5.27 to demonstrate
;; that both versions of the factorial procedure now require space
;; that grows linearly with their input.

;; Start the evaluator machine and run the driver loop:
(set-register-contents! ec-eval-machine 'flag false)
(start ec-eval-machine)


;; Iterative Factorial

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; (factorial 1) ; (total-pushes = 70 maximum-depth = 17)
;; (factorial 2) ; (total-pushes = 107 maximum-depth = 20)
;; (factorial 3) ; (total-pushes = 144 maximum-depth = 23)
;; (factorial 4) ; (total-pushes = 181 maximum-depth = 26)
;; (factorial 5) ; (total-pushes = 218 maximum-depth = 29)


;; Recursive Factorial

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; (factorial 1) ; (total-pushes = 18 maximum-depth = 11)
;; (factorial 2) ; (total-pushes = 52 maximum-depth = 19)
;; (factorial 3) ; (total-pushes = 86 maximum-depth = 27)
;; (factorial 4) ; (total-pushes = 120 maximum-depth = 35)
;; (factorial 5) ; (total-pushes = 154 maximum-depth = 43)


;; As the tests show, the maximum stack depth grows linearly
;; with the input in both versions of factorial given that
;; the evaluator is no longer tail-recursive.
