;; Two-dimensional table
;;
;; Each value is indexed by two keys.
;;
;; Such a table can be constructed as a one-dimensional table in which
;; each key identifies a subtable.
;;
;; The subtables don't need a special header symbol,
;; since the key that identifies the subtable serves this purpose.
;;
;; The box-and-pointer diagram for the table
;;
;; math:
;;     +: 43
;;     -: 45
;;     *: 42
;; letters:
;;     a: 97
;;     b: 98
;;
;; is shown below: 
;;
;; table
;;  |
;;  ↓
;; |x|x|----->|x|x|----->|x|/|
;;  |          |          |
;;  ↓          |          ↓
;; *table*     |         |x|x|----->|x|x|----->|x|/|
;;             |          |          | |        |
;;             |          ↓          ↓ ↓        ↓
;;             |         letters    |x|x|      |x|x|
;;             |                     | |        | |
;;             |                    'a 97      'b 98
;;             |
;;             ↓
;;            |x|x|----->|x|x|----->|x|x|----->|x|/|
;;             |          |          |          |
;;             ↓          ↓          ↓          ↓
;;            math       |x|x|      |x|x|      |x|x|
;;                        | |        | |        | |
;;                        + 43       - 45       * 42
;;
;;
;; Operations:
;;
;; - `lookup` - when an item is being looked up, the first key is used to
;;              identify the correct subtable.
;;              Then the second key is used to identify the record within
;;              the subtable.
;; 
;; - `insert!` - to insert a new item under a pair of keys, the operation
;;               finds out if there is a subtable stored under the first key.
;;               If not, a new subtable containing the single record (key-2, value)
;;               is built and inserted into the table under the first key.
;;               If a subtable already exists for the first key,
;;               the new record is inserted into this subtable,
;;               using the insertion method for one-dimensional tables.


(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else
         (assoc key (cdr records)))))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value)
                                       (cdr subtable)))))
        (set-cdr! table (cons (list key-1
                                    (cons key-2 value))
                              (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;; Tests
;;
;; (define t2 (make-table))
;;
;; t2 ; (*table*)
;;
;; (insert! 'letters 'b 98 t2)
;; (insert! 'letters 'a 97 t2)
;;
;; t2 ; (*table* (letters (a . 97) (b . 98)))
;;
;; (insert! 'math '* 42 t2)
;; (insert! 'math '- 45 t2)
;; (insert! 'math '+ 43 t2)
;;
;; t2 ; (*table* (math (+ . 43) (- . 45) (* . 42)) (letters (a . 97) (b . 98)))
;;
;; (lookup 'math '+ t2) ; 43
;; (lookup 'math '- t2) ; 45
;; (lookup 'math '* t2) ; 42
;;
;; (lookup 'letters 'a t2) ; 97
;; (lookup 'letters 'b t2) ; 98
;;
;; (lookup 'math 'NA t2)    ; false
;; (lookup 'letters 'NA t2) ; false
;; (lookup 'NA 'NA t2)      ; false
