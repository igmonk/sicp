;; Exercise 2.68
;;
;; The encode procedure takes as arguments a message and a tree
;; and produces the list of bits that gives the encoded message.

(load "workbook.scm")
(load "ex-2.67.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; encode-symbol is a procedure, which you must write, that returns
;; the list of bits that encodes a given symbol according to a given tree.
;;
;; You should design encode-symbol so that it signals an error if
;; the symbol is not in the tree at all.
;;
;; Test your procedure by encoding the result you obtained in ex. 2.67
;; with the sample tree and seeing whether it is the same as
;; the original sample message.

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '()) ; <-- can be another equality check here
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else
         (error "the symbol is not in the tree -- ENCODE-SYMBOL" symbol))))

;; encode-symbol makes use of the element-of-set? procedure defined for sets
;; represented by unordered lists.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define sample-message '(a d a b b c a))

;; (encode sample-message sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
