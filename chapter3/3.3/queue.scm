;; Queue (FIFO buffer)
;;
;; A queue is a sequence in which items are inserted at one end
;; (called the rear of the queue) and deleted from the other end (the front).
;;
;; A queue as defined by the following set of operations:
;;
;; - a constructor
;;   (make-queue)
;;   returns an empty queue
;;
;; - selectors:
;;   - (empty-queue? <queue>)
;;     tests if the queue is empty
;;   - (front-queue <queue>)
;;     returns the object at the front of the queue,
;;     signalling an error if the queue is empty
;;
;; - mutators:
;;   - (insert-queue! <queue> <item>)
;;     inserts the item at the rear of the queue and returns the modified queue
;;     as its value
;;   - (delete-queue! <queue>)
;;     removes the item at the front of the queue and returns the modified queue
;;     as its value, signaling an error if the queue is empty before the deletion
;;
;; A queue is represented as a pair of pointers, front-ptr and rear-ptr,
;; which indicate, respectively, the first and last pairs in an ordinary list.
;;
;; q --->|x|x|----------------------------↓
;;        |                               |
;;        | front-ptr                     | rear-ptr
;;        ↓                               ↓
;;       |x|x|---------->|x|x|---------->|x|/|
;;        |               |               |
;;        a               b               c


(define (make-queue)
  (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))


;; To insert an item in a queue:
;; - create a new pair whose car is the item to be inserted and
;;   whose cdr is empty list
;; - if the queue was initially empty, set the front and rear pointers
;;   of the queue to this new pair
;; - otherwise, modify the final pair in the queue to point to the new pair,
;;   and set the rear pointer to the new pair

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))


;; To delete the item at the front of the queue:
;; - if the queue was initially empty, signal an error
;; - otherwise, modify the front pointer so that it points at
;;   the second item in the queue

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; Print the queue items

(define (print-queue queue)
  (display (front-ptr queue)))


;; Tests
;;
;; (define q (make-queue))
;;
;; (insert-queue! q 'a) ; ((a) a)
;; (insert-queue! q 'b) ; ((a b) b)
;; (delete-queue! q)    ; ((b) b)
;; (insert-queue! q 'c) ; ((b c) c)
;; (insert-queue! q 'd) ; ((b c d) d)
;; (delete-queue! q)    ; ((c d) d)
;;
;; (print-queue q)      ; (c d)
