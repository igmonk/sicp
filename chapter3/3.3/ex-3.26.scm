;; Exercise 3.26
;;
;; To search a table as implemented above, one needs to scan through
;; the list of records. This is basically the unordered list representation
;; of section 2.3.3.
;;
;; For large tables, it may be more efficient to structure the table
;; in a different manner.
;;
;; Describe a table implementation where the (key, value) records
;; are organized using a binary tree, assuming that keys can be ordered
;; in some way (e.g., numerically or alphabetically).
;;
;; (Compare exercise 2.66 of chapter 2.)


(load "table-md-bst.scm")

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
