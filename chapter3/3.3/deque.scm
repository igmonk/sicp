;; A deque is represented as a pair of pointers, front-ptr and rear-ptr,
;; which indicate, respectively, the first and last pairs in an ordinary list.
;;
;; The car of each pair of the list in its turn is represented by a pair, whose
;; - car points to the inserted item
;; - cdr points to the previous pair of the list
;;
;; Such a representation is known as a doubly linked list, which is needed
;; when, while deleting the rear item of the deque, moving the rear pointer
;; to the next to last element.
;;
;; q --->|x|x|--------------------------------------------↓
;;        |                                               |
;;        | front-ptr     ↓---------------------↑         | rear-ptr
;;        |               |                     |         |
;;        |←--------------|-----↑         ↓-----|---------|-----↑
;;        ↓               ↓     |         ↓     |         ↓     |
;;       |x|x|---------->|x|x|--↑------->|x|x|--|------->|x|/|  |
;;        |               |     |         |     |         |     |
;;        |               |     |         |     |         |     |
;;        ↓               ↓     |         ↓     |         ↓     |
;;       |x|/|           |x|x|--↑        |x|x|--↑        |x|x|--↑
;;        |               |               |               |
;;        a               b               c               d


(define (make-deque)
  (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque! deque)
  (set-front-ptr! deque '())
  (set-rear-ptr! deque '()))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (caar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '())
                        (front-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item (rear-ptr deque))
                        '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (empty-deque! deque))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         (set-cdr! (car (front-ptr deque)) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque)) ; 1-element deque
         (empty-deque! deque))
        (else
         (set-rear-ptr! deque (cdar (rear-ptr deque)))
         (set-cdr! (rear-ptr deque) '())
         deque)))

(define (print-deque deque)
  (define (inner l)
    (cond ((null? l)
           (display ")"))
          (else
           (display " ")
           (display (caar l))
           (inner (cdr l)))))
  (display "(")
  (inner (front-ptr deque)))


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
