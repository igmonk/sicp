;; Exercise 3.9
;;
;; In section 1.2.1 we used the substitution model to analyze two procedures
;; for computing factorials, a recursive version
;;
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; and an iterative version

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Show the environment structures created by evaluating (factorial 6)
;; using each version of the factorial procedure.


;; Consider (factorial 3)


;; 1. Recursive
;;
;; --------------------------------------------------------------------------------
;;
;;  Global
;;  Env      factorial--↓
;;                      |
;; ---------------------|----------------------------------------------------------
;;                      |    ↑       ↑                ↑                ↑
;;                      ↓    |       |                |                |
;;                    |x|x|--↑     E1|n:3           E2|n:2           E3|n:1
;;                     |             |<factorial>     |<factorial>     |<factorial>
;;                     ↓
;;           parameters: n
;;           body: <factorial>


;; 2. Iterative
;;
;; --------------------------------------------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env      factorial--↓          fact-iter--↓
;;                      |                     |
;; ---------------------|---------------------|------------------------------------------------------------------------------------------------
;;                      |    ↑                |    ↑           ↑                ↑                ↑                ↑                ↑
;;                      ↓    |                ↓    |           |                |                |                |                |
;;                    |x|x|--↑              |x|x|--↑         E1|n:3           E2|product:1     E3|product:1     E4|product:2     E5|product:6
;;                     |                     |                 |<factorial>     |counter:1       |counter:2       |counter:3       |counter:4
;;                     ↓                     ↓                                  |max-count:3     |max-count:3     |max-count:3     |max-count:3
;;           parameters: n         parameters: product                          |<fact-iter>     |<fact-iter>     |<fact-iter>     |<fact-iter>
;;           body: <factorial>                 counter
;;                                             max-count
;;                                 body: <fact-iter>
