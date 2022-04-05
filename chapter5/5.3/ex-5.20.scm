;; Exercise 5.20
;;
;; Draw the box-and-pointer representation and the memory-vector
;; representation (as in figure 5.14) of the list structure
;; produced by

(define x (cons 1 2))
(define y (list x x))

;; with the 'free' pointer initially 'p1'.
;;
;; What is the final value of 'free'?
;; What pointers represent the values of x and y?


;; Box-and-pointer representation
;;
;; y ----->|x|x|----->|x|/|
;;       (3)|       (2)|
;;          |----------┘
;;          ↓
;; x ----->|x|x|
;;       (1)| |
;;          ↓ ↓
;;          1 2


;; Memory-vector representation
;;
;;    Index | 0 |  1 |  2 |  3 | 4 |
;; the-cars |   | n1 | p1 | p1 |   |
;; the-cdrs |   | n2 | e0 | p2 |   |


;;    x = p1 (index 1)
;;    y = p3 (index 3)
;; free = p4 (index 4)


;; Notice, the address of a pair resulted in cons-ing its arguments
;; in the-cars and the-cdrs becomes available only after the
;; constituents have been stored in memory:
;;
;; (perform
;;   (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
;; (perform
;;   (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
;; (assign <reg1> (reg free))
;; (assign free (op +) (reg free) (const 1))
;;
;; However, nothing stops us from storing the value of free in
;; the target register before accessing it in vector-set! operations:
;;
;; (assign <reg1> (reg free))
;; (perform
;;   (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
;; (perform
;;   (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
;; (assign free (op +) (reg free) (const 1))
