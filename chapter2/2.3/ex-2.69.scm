;; Exercise 2.69
;;
;; The following procedure takes as its argument a list of symbol-frequency pairs
;; (where no symbol appears in more than one pair) and generates a Huffman encoding tree
;; according to the Huffman algorithm.

(load "workbook.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; make-leaf-set is the procedure given above that transforms the list of pairs
;; into an ordered set of leaves.
;;
;; successive-merge is the procedure you must write, using make-code-tree
;; to successively merge the smallest-weight elements of the set
;; until there is only one element left, which is the desired Huffman tree.
;;
;; (This procedure is slightly tricky, but not really complicated.
;; If you find yourself designing a complex procedure,
;; then you are almost certainly doing something wrong.
;; You can take significant advantage of the fact that we are using
;; an ordered set representation.)

(define (successive-merge set-of-nodes)
  (cond ((null? set-of-nodes) '())
        ((= (length set-of-nodes) 1)
         (car set-of-nodes))
        (else
         (successive-merge
          (adjoin-set (make-code-tree (car set-of-nodes)
                                      (cadr set-of-nodes))
                      (cddr set-of-nodes))))))

;; At each recursive call (the else branch) the first two items
;; of the ordered (ASC) set of nodes are taken to form a tree node,
;; which is then adjoined to the remaining set items.
;;
;; The order in which the nodes are arranged is preserved and guaranteed by
;; the adjoin-set procedure.
;;
;; As soon as there is only one element left, it is returned. 


;; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;;
;; Expected: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
;;           (the same as sample-tree defined in ex. 2.67)
;;
;; Actual:   ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)


;; (generate-huffman-tree '((a 7) (b 6) (c 5) (d 4) (e 3) (f 2) (g 1)))
;;
;; (((leaf b 6)
;;   ((leaf e 3)
;;    ((leaf g 1)
;;     (leaf f 2)
;;     (g f) 3)
;;    (e g f) 6)
;;   (b e g f) 12)
;;  ((leaf a 7)
;;   ((leaf d 4)
;;    (leaf c 5)
;;    (d c) 9)
;;   (a d c) 16)
;;  (b e g f a d c) 28)
