;; Exercise 1.26
;;
;; Louis Reasoner is having great difficulty doing exercise 1.24.
;; His fast-prime? test seems to run more slowly than his prime? test.
;; Louis calls his friend Eva Lu Ator over to help.
;; When they examine Louis's code, they find that he has rewritten the expmod procedure
;; to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; "I don't see what difference that could make," says Louis.
;; "I do." says Eva. "By writing the procedure like that,
;; you have transformed the θ(log n) process into a θ(n) process."
;;
;; Explain.


;; With the original implementation of expmod, the complexity is addressed by
;; successive squaring that halves the problem with each expmod call, which
;; results in the θ(log n) process.
;;
;; With Louis's implementation of expmod, together with halving the problem it is
;; being doubled due to the additional expmod call, which essentially transforms
;; the θ(log n) process back to the θ(n) one.
