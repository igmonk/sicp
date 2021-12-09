;; Exercise 3.47
;;
;; A semaphore (of size n) is a generalization of a mutex.
;;
;; Like a mutex, a semaphore supports acquire and release operations,
;; but it is more general in that up to n processes can acquire it concurrently.
;;
;; Additional processes that attempt to acquire the semaphore
;; must wait for release operations.
;;
;; Give implementations of semaphores
;;
;; a. in terms of mutexes
;;
;; b. in terms of atomic test-and-set! operations.


;; a. Semaphores in terms of mutexes
;;
;; The inner mutex acts as a guard when accessing and
;; setting the value of the inner count, which reflects
;; the number of processes acquired the semaphore.

(define (make-semaphore n)
  (let ((cnt 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< cnt n)
                 (begin (set! cnt (+ cnt 1))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'acquire)))) ;; retry
            ((eq? m 'release)
             (mutex 'acquire)
             (when (> cnt 0)
               (set! cnt (- cnt 1)))
             (mutex 'release))))
    the-semaphore))


;; b. Semaphores in terms of atomic test-and-set! operations
;;
;; The inner cell (a single-element list) acts as a guard
;; when accessing and setting the value for the inner count,
;; which reflects the number of processes acquired the semaphore.

(define (make-semaphore n)
  (let ((cnt 0)
        (cell (list false)))
    (define (test-and-set-cell!)
      (when (test-and-set! cell)
        (test-and-set-cell!)))
    (define (clear-cell!)
      (set-car! cell false))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (test-and-set-cell!)
             (if (< cnt n)
                 (begin (set! cnt (+ cnt 1))
                        (clear-cell!))
                 (begin (clear-cell!)
                        (the-semaphore 'acquire)))) ;; retry
            ((eq? m 'release)
             (test-and-set-cell!)
             (when (> cnt 0)
               (set! cnt (- cnt 1)))
             (clear-cell!))))
    the-semaphore))


;; Tests
;;
;; https://mitpress.mit.edu/sites/default/files/sicp/psets/ps7/readme.html

;; (load "parallel.scm")
;; (load-option 'format)

;; (define f1 (ns (make-fn 1)))
;; (define f2 (ns (make-fn 2)))
;; (define f3 (ns (make-fn 3)))
;; (define f4 (ns (make-fn 4)))
;; (define f5 (ns (make-fn 5)))
;; (define f6 (ns (make-fn 6)))

;; (parallel-execute f1 f2 f3 f4 f5 f6)
;;
;; P1: START, time: 24:32
;; P2: START, time: 24:32
;; P3: START, time: 24:32
;; P4: START, time: 24:32
;; P1: END, time: 24:34
;; P2: END, time: 24:35
;; P3: END, time: 24:35
;; P4: END, time: 24:35
;; P6: START, time: 24:36
;; P5: START, time: 24:36
;; P6: END, time: 24:38
;; P5: END, time: 24:38
;;
;; Since the semaphore's size is 4, only the first four processes
;; were able to acquire the semaphore (P1, P2, P3, P4), while
;; the rest two (P5 and P6) had to wait for 2 seconds before
;; it was released.


;; Test utils

(define (make-n-serializer n)
  (let ((semaphore (make-semaphore n)))
    (lambda (p)
      (define (serialized-p . args)
        (semaphore 'acquire)
        (let ((val (apply p args)))
          (semaphore 'release)
          val))
      serialized-p)))

(define ns (make-n-serializer 4))

(define (time-min-sec)
  (format false "~a:~a"
          (decoded-time/minute (global-decoded-time))
          (decoded-time/second (global-decoded-time))))

(define (make-fn n)
  (lambda ()
    (newline)
    (display (format false "P~a: START, time: ~a" n (time-min-sec)))
    (sleep-current-thread 2000)
    (newline)
    (display (format false "P~a: END, time: ~a" n (time-min-sec)))))
