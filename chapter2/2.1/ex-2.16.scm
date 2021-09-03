;; Exercise 2.16
;;
;; Explain, in general, why equivalent algebraic expressions may lead to different answers.
;; Can you devise an interval-arithmetic package that does not have this shortcoming,
;; or is this task impossible? (Warning: This problem is very difficult.)


;; As stated in ex. 2.15:
;;
;;   Algebraic equivalence of two expressions involving numbers
;;   does not automatically mean that of two expressions involving intervals.
;;
;;   The whole set of algebraic operations, transformations and laws (commutative, associative,
;;   distributive) have to be redefined all together when working with intervals.
;;
;;   Until that is done, the overall error will be cumulative, depending on how many
;;   interval operations are to be performed: the more operations, the bigger error
;;   (or a bigger discrepancy from what could be expected from the algebraic perspective).
;;
;; Hence, an interval-arithmetic package that does not have the shortcoming described above,
;; must provide all the necessary operations, transformations and laws equivalent to
;; their algebraic counterparts.
