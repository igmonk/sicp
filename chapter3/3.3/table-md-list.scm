;; Multidimensional Table / List
;;
;; A multidimensioanl table where the (key, value) records are organized using
;; a list structure.
;;
;; Different values may be stored under different numbers of keys.

(define (make-table)

  ;; Value-Dimension pair
  ;;
  ;; In order to allow for keeping both the inserted value and
  ;; a possible entrance to another dimension, each item of the table
  ;; is represented as a pair of the following structure:
  ;;       Value -> a piece of data containing whatever is inserted
  ;;   Dimension -> a structure representing the inner dimension
  (define (make-vd value dimension) (cons value dimension))
  (define (vd-value vd) (car vd))
  (define (vd-dimension vd) (cdr vd))
  (define (set-vd-value! vd value) (set-car! vd value))
  
  ;; internal procedures
  (define (assoc key records)
    (cond ((null? records) false)
          ((not (pair? records)) false)
          ((equal? key (caar records)) (car records))
          (else
           (assoc key (cdr records)))))

  (define (assoc-v key records)
    (let ((record (assoc key records)))
      (and record (cdr record))))
  
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
        (let ((vd (assoc-v (car ks) (cdr table))))
          (cond ((not vd) false)
                ((null? (cdr ks)) (vd-value vd))
                (else
                 (inner (cdr ks) (vd-dimension vd))))))
      (if (null? keys)
          false
          (inner keys local-table)))
    
    ;; Multi-dimensional insert! breaks the task into 4 pieces.
    ;;
    ;; If there is
    ;; 1) an item for the final key.   => Update
    ;; 2) no item for the final key.   => Insert
    ;; 3) an item for a non-final key. => Recursive call with the rest of keys
    ;;                                    and item's dimension
    ;; 4) no item for a non-final key. => Create a new item with false as its value
    ;;                                    and a list as its dimension.
    ;;                                    Recursive call with the rest of keys
    ;;                                    and newly created item's dimension.
    (define (insert! keys value)
      (define (inner ks table)
        (let ((final-key? (null? (cdr ks)))
              (vd (assoc-v (car ks) (cdr table))))
          (cond ((and final-key? vd)
                 (set-vd-value! vd value))
                ((and final-key? (not vd))
                 (set-cdr! table (cons (cons (car ks)
                                             (make-vd value (list '*table*)))
                                       (cdr table))))
                ((and (not final-key?) vd)
                 (inner (cdr ks) (vd-dimension vd)))
                (else
                 (let ((vd (make-vd false (list '*table*))))
                       (set-cdr! table (cons (cons (car ks) vd)
                                             (cdr table)))
                       (inner (cdr ks) (vd-dimension vd)))))))
      (if (null? keys)
          (error "Empty key list -- INSERT")
          (inner keys local-table))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; Tests
;;
;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; (get (list 'one))      ; false
;; (get (list 'one 'two)) ; false

;; (put (list 'one) 1)

;; (get (list 'one))      ; 1
;; (get (list 'one 'two)) ; false

;; (put (list 'one 'two) 12)

;; (get (list 'one))      ; 1
;; (get (list 'one 'two)) ; 12

;; (put (list 'a 'b 'c) "abc")

;; (get (list 'a))       ; false
;; (get (list 'a 'b))    ; false
;; (get (list 'a 'b 'c)) ; "abc"
