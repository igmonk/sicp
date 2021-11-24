;; Exercise 3.25
;;
;; Generalizing one- and two-dimensional tables, show how to implement
;; a table in which values are stored under an arbitrary number of keys
;; and different values may be stored under different numbers of keys.
;;
;; The lookup and insert! procedures should take as input a list of keys
;; used to access the table.


(load "table-md-list.scm")

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
