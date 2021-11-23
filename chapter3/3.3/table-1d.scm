;; One-dimentional table
;;
;; Each value is stored under a single key.
;;
;; The table is implemneted as a list of records, each of which
;; is implemented as a pair consisting of a key and the associated value.
;;
;; The records are glued together to form a list by pairs whose cars
;; point to successive records.
;; These gluing pairs are called the 'backbone' of the table.
;;
;; In order to have a place that can be changed when a new record is added
;; to the table, the table is built as a headed list.
;; A headed list has a special backbone pair at the beginning,
;; which holds a dummy "record" - in this case the arbitrarily chosen
;; symbol *table*.
;;
;; table
;;  |
;;  ↓
;; |x|x|----->|x|x|----->|x|x|----->|x|/|
;;  |          |          |          |
;;  ↓          ↓          ↓          ↓
;; *table*    |x|x|      |x|x|      |x|x|
;;             | |        | |        | |
;;            'a 1       'b 2       'c 3
;;
;;
;; Operations:
;;
;; - `assoc` [auxiliary]
;;   returns the record that has the given key as its `car`
;;
;; - `lookup` [extracts information from a table]
;;   takes a key as argument and returns the associated value
;;   (or `false` if there is no value stored under that key)
;;
;; - `insert!` [inserts information in a table]
;;   inserts a value in a table under specified key


(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else
         (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;; Tests
;;
;; (define t1 (make-table))
;;
;; t1 ; (*table*)
;;
;; (insert! 'c 3 t1)
;; (insert! 'b 2 t1)
;; (insert! 'a 1 t1)
;;
;; t1 ; (*table* (a . 1) (b . 2) (c . 3))
;;
;; (lookup 'a t1) ; 1
;; (lookup 'b t1) ; 2
;; (lookup 'c t1) ; 3
;; (lookup 'd t1) ; false
