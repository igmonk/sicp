;; Exercise 2.55
;;
;; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra) ; quote

;; To her surprise, the interpreter prints back quote. Explain.


;; Note that

(car ''abracadabra) ; quote

;; is equivalent to

(car '(quote abracadabra)) ; quote

;; which is an ordinary application of car to a list, whose 1st element is 'quote'.
