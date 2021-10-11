;; Exercise 2.66
;;
;; Implement the 'lookup' procedure for the case where the set of records
;; is structured as a binary tree, ordered by the numerical values of the keys.

(load "workbook.scm")

(define (lookup key tree)
  (if (null? tree)
      false
      (let ((record (entry tree)))
        (cond ((equal? key (record-key record))
               record)
              ((< key (record-key record))
               (lookup key (left-branch tree)))
              ((> key (record-key record))
               (lookup key (right-branch tree)))))))


;; Data representation for records

(define (make-record key value)
  (list key value))

(define (record-key record)
  (car record))

(define (record-value record)
  (cadr record))

;; (lookup 1 '()) ; false
;;
;; (define t1
;;   (make-tree (make-record 7 "seven")
;;              (make-tree (make-record 3 "three")
;;                         (make-tree (make-record 1 "one") '() '())
;;                         (make-tree (make-record 5 "five") '() '()))
;;              (make-tree (make-record 9 "nine")
;;                         '()
;;                         (make-tree (make-record 11 "eleven") '() '()))))
;;
;; (lookup 1 t1)  ; (1 "one")
;; (lookup 2 t1)  ; false
;; (lookup 3 t1)  ; (3 "three")
;; (lookup 4 t1)  ; false
;; (lookup 5 t1)  ; (5 "five")
;; (lookup 6 t1)  ; false
;; (lookup 7 t1)  ; (7 "seven")
;; (lookup 8 t1)  ; false
;; (lookup 9 t1)  ; (9 "nine")
;; (lookup 10 t1) ; false
;; (lookup 11 t1) ; (11 "eleven")
;; (lookup 12 t1) ; false
