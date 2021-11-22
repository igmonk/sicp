;; Exercise 3.23
;;
;; A deque ("double-ended queue") is a sequence in which items
;; can be inserted and deleted at either the front or the rear.
;;
;; Operations on deques are:
;; - the constructor make-deque
;; - the predicate empty-deque?
;; - selectors front-deque and rear-deque
;; - mutators front-insert-deque!, rear-insert-deque!,
;;            front-delete-deque!, rear-delete-deque!
;;
;; Show how to represent deques using pairs
;; and give implementations of the operations.
;;
;; Be careful not to make the interpreter try to print a structure
;; that contains cycles. 
;;
;; All operations should be accomplished in θ(1) steps.


(load "deque.scm")

;; Tests
;;
;; (define d1 (make-deque))
;;
;; (print-deque d1) ; ()
;;
;; (front-insert-deque! d1 'a)
;; (front-insert-deque! d1 'b)
;; (front-insert-deque! d1 'c)
;;
;; (print-deque d1) ; (c b a)
;;
;; (rear-insert-deque! d1 'x)
;; (rear-insert-deque! d1 'y)
;; (rear-insert-deque! d1 'z)
;;
;; (print-deque d1) ; (c b a x y z)
;;
;; (front-delete-deque! d1)
;; (front-delete-deque! d1)
;;
;; (print-deque d1) ; (a x y z)
;;
;; (rear-delete-deque! d1)
;; (rear-delete-deque! d1)
;;
;; (print-deque d1) ; (a x)
;;
;; (front-delete-deque! d1)
;; (rear-delete-deque! d1)
;;
;; (print-deque d1) ; ()
