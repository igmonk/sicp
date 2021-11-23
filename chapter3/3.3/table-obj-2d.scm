;; Two-dimensional table object
;;
;; A table is represented procedurally, as an object that
;; maintains an internal table as part of its local state.
;;
;; When sent appropriate message, this "table object" supplies
;; the procedure with which to operate on the internal table.

(define (make-table)

  ;; internal procedures
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else
           (assoc key (cdr records)))))

  ;; object's procedures
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1
                                              (cons key-2 value))
                                        (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; Using make-table, we could implement the get and put operations
;; used in section 2.4.3 for data-directed programming, as follows:

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; 'get' takes as arguments two keys, and 'put' takes as arguments
;; two keys and a value.
;;
;; Both operations access the same local table, which is encapsulated
;; within the object created by the call to 'make-table'.


;; Tests
;;
;; (put 'primitive '+ +)
;; (put 'primitive '* *)
;;
;; (define (factorial x)
;;   (define (inner acc n)
;;     (if (> n 1)
;;         (inner (* n acc) (- n 1))
;;         acc))
;;   (inner 1 x))
;;
;; (put 'complex 'factorial factorial)
;;
;; ((get 'primitive '+) 1 2 3 4 5) ; 15
;; ((get 'primitive '*) 1 2 3 4 5) ; 120
;;
;; ((get 'complex 'factorial) 5) ; 120
