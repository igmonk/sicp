;; Exercise 3.24
;;
;; In the table implementations above, the keys are tested for equality
;; using equal? (called by assoc).
;;
;; This is not always the appropriate test.
;; For instance, we might have a table with numeric keys in which
;; we don't need an exact match to the number we're looking up,
;; but only a number within some tolerance of it.
;;
;; Design a table constructor make-table that takes as an argument
;; a same-key? procedure that will be used to test "equality" of keys.
;;
;; 'make-table' should return a dispatch procedure that can be used to
;; access appropriate lookup and insert! procedures for a local table.

(define (make-table same-key?)

  ;; internal procedures
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
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
                                        (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; non-exact number equality check

(define (diff-less-than-5? n1 n2)
  (< (abs (- n1 n2)) 5))

;; get and put operations

(define operation-table (make-table diff-less-than-5?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 'get' takes as arguments two keys, and 'put' takes as arguments
;; two keys and a value.
;;
;; Both operations access the same local table, which is encapsulated
;; within the object created by the call to 'make-table'.


;; Tests
;;
;; (put 10 100 'a)
;; (put 15 105 'b)
;; (put 20 110 'c)

;; (get 5 100)  ; false
;; (get 6 100)  ; a
;; (get 7 100)  ; a
;; (get 8 100)  ; a
;; (get 9 100)  ; a
;; (get 10 100) ; a
;; (get 11 100) ; a
;; (get 12 100) ; a
;; (get 13 100) ; a
;; (get 14 100) ; a
;; (get 15 100) ; false

;; (get 10 95)  ; false
;; (get 10 96)  ; a
;; (get 10 97)  ; a
;; (get 10 98)  ; a
;; (get 10 99)  ; a
;; (get 10 100) ; a
;; (get 10 101) ; a
;; (get 10 102) ; a
;; (get 10 103) ; a
;; (get 10 104) ; a
;; (get 10 105) ; false


;; (get 15 100) ; false
;;
;; (get 15 104) ; b
;; (get 15 105) ; b
;; (get 15 106) ; b
;;
;; (get 15 110) ; false


;; (get 20 105) ; false
;;
;; (get 20 109) ; c
;; (get 20 110) ; c
;; (get 20 111) ; c
;;
;; (get 20 115) ; false
