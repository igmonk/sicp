;; Multi-dimensional table / List / Fixed number of keys
;;
;; A table in which different values are stored
;; under a specified number of keys.
;;
;; The lookup and insert! procedures take as input a list of keys
;; used to access the table.


;; format is a run-time-loadable option. Source:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Format.html
(load-option 'format)

(define (make-table num-of-keys)
  
  ;; internal procedures
  (define (assoc key records)
    (cond ((null? records) false)
          ((not (pair? records)) false)
          ((equal? key (caar records)) (car records))
          (else
           (assoc key (cdr records)))))

  (define (check-keys ks)
    (if (or (null? ks) (not (= num-of-keys (length ks))))
        (error (format false "Wrong number of keys: ~a. Expected: ~a."
                       (length ks) num-of-keys))))
  
  ;; object's procedures
  (let ((local-table (list '*table*)))
    
    ;; Return false if there are no keys provided, otherwise
    ;; the task is broken into 2 parts:
    ;;
    ;; 1) 1 key   => The final dimension to search for the value.
    ;;               Return the value for the key (or false if not found).
    ;; 2) > 1 key => Search for the value (using the 1st key) in current dimension.
    ;;               If found, return the result of search in the subdimension
    ;;               using the rest of keys.
    (define (lookup keys)
      (define (inner ks table)
        (let ((record (assoc (car ks) (cdr table))))
          (cond ((not record) false)
                ((null? (cdr ks)) (cdr record))
                (else
                 (inner (cdr ks) record)))))
      (check-keys keys)
      (inner keys local-table))

    
    ;; Multi-dimensional insert! breaks the task into 4 pieces.
    ;;
    ;; If there is
    ;; 1) an item for the final key.   => Update
    ;; 2) no item for the final key.   => Insert
    ;; 3) an item for a non-final key. => Recursive call with the rest of keys
    ;;                                    and item's dimension
    ;; 4) no item for a non-final key. => Create a new item with false as its value
    ;;                                    and a BST as its dimension.
    ;;                                    Recursive call with the rest of keys
    ;;                                    and newly created item's dimension.
    (define (insert! keys value)
      (define (inner ks table)
        (let ((final-key? (null? (cdr ks)))
              (record (assoc (car ks) (cdr table))))
          (cond ((and final-key? record)
                 (set-cdr! record value))
                ((and final-key? (not record))
                 (set-cdr! table (cons (cons (car ks) value)
                                       (cdr table))))
                ((and (not final-key?) record)
                 (inner (cdr ks) record))
                (else
                 (let ((new-subtable (list (car ks))))
                       (set-cdr! table (cons new-subtable (cdr table)))
                       (inner (cdr ks) new-subtable))))))
      (check-keys keys)
      (inner keys local-table))
    
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; Tests
;;
;; 1. num-of-keys = 3
;;
;; (define operation-table (make-table 3))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; (get (list 'one))   ; Error: Wrong number of keys: 1. Expected: 3
;; (put (list 'one) 1) ; Error: Wrong number of keys: 1. Expected: 3

;; (get (list 'one 'two 'three)) ; false

;; (put (list 'one 'two 'three) 123)
;;
;; (get (list 'one 'two 'three)) ; 123

;; (put (list 'one 'two 'four) 124)
;;
;; (get (list 'one 'two 'three)) ; 123
;; (get (list 'one 'two 'four))  ; 124

;; (put (list 'five 'six 'seven) 567)
;;
;; (get (list 'one 'two 'three))  ; 123
;; (get (list 'one 'two 'four))   ; 124
;; (get (list 'five 'six 'seven)) ; 567

;; (put (list 'five 'six 'seven) 999)
;;
;; (get (list 'one 'two 'three))  ; 123
;; (get (list 'one 'two 'four))   ; 124
;; (get (list 'five 'six 'seven)) ; 999


;; 2. num-of-keys = 5
;;
;; (define operation-table (make-table 5))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; (get (list 'one))   ; Error: Wrong number of keys: 1. Expected: 5
;; (put (list 'one) 1) ; Error: Wrong number of keys: 1. Expected: 5

;; (get (list 'one 'two 'three 'four 'five)) ; false

;; (put (list 'one 'two 'three 'four 'five) 12345)
;;
;; (get (list 'one 'two 'three 'four 'five)) ; 12345

;; (put (list 'one 'two 'three 'four 'six) 12346)
;;
;; (get (list 'one 'two 'three 'four 'five)) ; 12345
;; (get (list 'one 'two 'three 'four 'six))  ; 12346

;; (put (list 'five 'four 'three 'two 'one) 54321)
;;
;; (get (list 'one 'two 'three 'four 'five)) ; 12345
;; (get (list 'one 'two 'three 'four 'six))  ; 12346
;; (get (list 'five 'four 'three 'two 'one)) ; 54321

;; (put (list 'five 'four 'three 'two 'one) 99999)
;;
;; (get (list 'one 'two 'three 'four 'five)) ; 12345
;; (get (list 'one 'two 'three 'four 'six))  ; 12346
;; (get (list 'five 'four 'three 'two 'one)) ; 99999
