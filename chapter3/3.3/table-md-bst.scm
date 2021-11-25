;; Multidimensional Table / Binary Search Tree
;;
;; A multidimensioanl table where the (key, value) records are organized using
;; a binary search tree with a specified comparator.
;;
;; Different values may be stored under different numbers of keys.
;;
;; Comparators: https://srfi.schemers.org/srfi-128/srfi-128.html


(load "bst.scm")

(define (make-table comp)
  
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
  
  ;; Local state
  (let ((local-table (make-bst comp)))

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
        (let ((vd ((table 'lookup) (car ks))))
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
    ;;                                    and a BST as its dimension.
    ;;                                    Recursive call with the rest of keys
    ;;                                    and newly created item's dimension.
    (define (insert! keys value)
      (define (inner ks table)
        (let ((final-key? (null? (cdr ks)))
              (vd ((table 'lookup) (car ks))))
          (cond ((and final-key? vd)
                 (set-vd-value! vd value))
                ((and final-key? (not vd))
                 ((table 'insert!) (car ks) (make-vd value (make-bst comp))))
                ((and (not final-key?) vd)
                 (inner (cdr ks) (vd-dimension vd)))
                (else
                 (let ((vd (make-vd false (make-bst comp))))
                    ((table 'insert!) (car ks) vd)
                    (inner (cdr ks) (vd-dimension vd)))))))
      (if (null? keys)
          (error "Empty key list -- INSERT")
          (inner keys local-table))
      'ok)
    
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; Tests
;;
;; 1. Symbolic keys
;;
;; (define symbol-comp (make-comparator symbol? symbol=? symbol<? symbol-hash))

;; (define operation-table (make-table symbol-comp))
;; (define get (operation-table 'lookup))
;; (define put (operation-table 'insert!))

;; (get '())         ; false
;; (get (list 'one)) ; false
;; (get (list 'two)) ; false

;; (put (list 'one) 1) ; ok
;; (get (list 'one))   ; 1

;; (put (list 'two) 2) ; ok
;; (get (list 'two))   ; 2

;; (get (list 'one 'two 'three 'four 'five)) ; false
;; (get (list 'one 'two 'three 'four))       ; false

;; (put (list 'one 'two 'three 'four 'five) 12345) ; ok
;; (get (list 'one 'two 'three 'four 'five))       ; 12345
;; (get (list 'one 'two 'three 'four))             ; false

;; (put (list 'one 'two 'three 'four) 1234)  ; ok
;; (get (list 'one 'two 'three 'four))       ; 1234
;; (get (list 'one 'two 'three 'four 'five)) ; 12345

;; (put (list 'a 'b 'c) "abc")

;; (get (list 'a))       ; false
;; (get (list 'b))       ; false
;; (get (list 'c))       ; false
;; (get (list 'a 'b))    ; false
;; (get (list 'a 'b 'c)) ; "abc"


;; 2. Numeric keys
;;
;; (define int-comp (make-comparator integer? = < number-hash))

;; (define operation-table-2 (make-table int-comp))
;; (define get (operation-table-2 'lookup))
;; (define put (operation-table-2 'insert!))

;; (get '())      ; false
;; (get (list 1)) ; false
;; (get (list 2)) ; false

;; (put (list 1) 10) ; ok
;; (get (list 1))    ; 10

;; (put (list 2) 20) ; ok
;; (get (list 2))    ; 20

;; (get (list 1 2 3 4 5)) ; false
;; (get (list 1 2 3 4))   ; false

;; (put (list 1 2 3 4 5) 123450) ; ok
;; (get (list 1 2 3 4 5))        ; 123450
;; (get (list 1 2 3 4))          ; false

;; (put (list 1 2 3 4) 12340) ; ok
;; (get (list 1 2 3 4))       ; 12340
;; (get (list 1 2 3 4 5))     ; 123450
