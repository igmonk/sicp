;; Exercise 3.19
;;
;; Redo exercise 3.18 using an algorithm that takes only a constant amount of space.
;; (This requires a very clever idea.)


;; A very clever idea can be nothing much than marking each visited item.
;;
;; Since each item is represented as a pair, there are two
;; possible alternatives:
;; 1. Marking car-s
;; 2. Marking cdr-s
;;
;; Hence, a list contains a cycle if at any step of its passage,
;; an item with the marking is met.


;; 1. Mark the car of each visited item.
;;
;; The flaw of the solution becomes apparent when an item
;; contains the same symbol the procedure uses for marking
;; visited items.

(define (contains-cycle? x)
  (cond ((null? x) false)
        ((eq? 'visited (car x)) true)
        (else
         (set-car! x 'visited)
         (contains-cycle? (cdr x)))))


;; 2. Mark the cdr of each visited item.

(define (contains-cycle? x)
  (cond ((null? x) false)
        ((eq? 'visited (cdr x)) true)
        (else
         (let ((x-cdr (cdr x)))
           (set-cdr! x 'visited)
           (contains-cycle? x-cdr)))))


;; Another idea that helps to find out if a list has a cycle,
;; is traversing the list using two pointers:
;; 1) one that uses sequential 1-step advancement using cdr, and
;; 2) another that uses sequential 2-step advancement using cddr.
;;
;; If the list contains a cycle, these pointers will eventually meet.
;; (they should meet at a place that belong to the cycle)
;;
;; Such an approach does not require to alter the given data structure.

(define (contains-cycle? x)
  (define (inner slow fast)
    (cond ((null? fast) false)
          ((null? (cdr fast)) false)
          ((eq? fast slow) true)
          (else
           (inner (cdr slow) (cddr fast)))))
  (if (null? x)
      false
      (inner x (cdr x))))


;; (define l1 (list 'a 'b 'c))
;; (define l2 (list 'd 'e 'f))

;; (set-cdr! (cddr l2) l2)

;; (contains-cycle? '()) ; false
;; (contains-cycle? l1)  ; false
;; (contains-cycle? l2)  ; true
