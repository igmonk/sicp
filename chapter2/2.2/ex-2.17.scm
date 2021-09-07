;; Exercise 2.17
;;
;; Define a procedure 'last-pair' that returns the list that contains
;; only the last element of a given (nonempty) list:
;;
;; (last-pair (list 23 72 149 34)) ; (34)

(define (last-pair items)
  (if (or (null? items)
          (null? (cdr items)))
      items
      (last-pair (cdr items))))

;; (last-pair (list 23 72 149 34)) ; (34)
;; (last-pair (list 1))            ; (1)
;; (last-pair '())                 ; ()
