;; Huffman Tree
;;
;; A Huffman code can be represented as a binary tree whose leaves
;; are the symbols that are encoded.
;;
;; At each non-leaf node of the tree there is a set containing all the symbols
;; in the leaves that lie below the node.
;;
;; In addition, each symbol at a leaf is assigned a weight (which is its relative frequency),
;; and each non-leaf node contains a weight that is the sum of all the weights of
;; the leaves lying below it.
;;
;; The weights are not used in the encoding or the decoding process.


;; Representation
;;
;; When we make a tree by merging two nodes,
;; we obtain the weight of the tree as the sum of the weights of the nodes,
;; and the set of symbols as the union of the sets of symbols for the nodes.

(define (make-node left right symbols weight)
  (list left right symbols weight))

(define (make-leaf symbol weight)
  (make-node '() '() (list symbol) weight))

(define (left node) (car node))
(define (right node) (cadr node))
(define (symbols node) (caddr node))
(define (weight node) (cadddr node))

(define (leaf? node)
  (and (null? (left node))
       (null? (right node))))

(define (symbol-leaf node)
  (car (symbols node)))

(define (merge-nodes left right)
  (make-node left
             right
             (append (symbols left) (symbols right))
             (+ (weight left) (weight right))))


;; Decoding
;;
;; The procedure keeps moving down the tree,
;; choosing a left or a right branch according to
;; whether the next bit in the list is a zero or a one.
;;
;; When it reaches a leaf, it returns the symbol at that leaf
;; as the next symbol in the message by cons-ing it onto the result
;; of decoding the rest of the message, starting at the root of the tree.

(define (decode bits tree)
  (define (decode-1 bits node)
    (if (null? bits)
        '()
        (let ((next-node (choose-branch (car bits) node)))
          (if (leaf? next-node)
              (cons (symbol-leaf next-node)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-node)))))
  (decode-1 bits tree))


;; Encoding
;;
;; The encode procedure takes as arguments a message and a tree
;; and produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left tree)))
         (cons 0 (encode-symbol symbol (left tree))))
        ((element-of-set? symbol (symbols (right tree)))
         (cons 1 (encode-symbol symbol (right tree))))
        (else
         (error "The symbol is not in the tree -- ENCODE-SYMBOL" symbol))))


;; Huffman tree generation
;;
;; The following procedure generates a Huffman encoding tree
;; according to the Huffman algorithm:

(define (generate-huffman-tree pairs)
  (successive-merge (make-node-set pairs)))

;; The following procedure uses merge-nodes to successively merge
;; the smallest-weight elements of the set until there is only
;; one element left, which is the desired Huffman tree.

(define (successive-merge nodes)
  (cond ((null? nodes) '())
        ((= 1 (length nodes)) (car nodes))
        (else
         (successive-merge
          (adjoin-set (merge-nodes (car nodes) (cadr nodes))
                      (cddr nodes))))))


;; Utils

(define (choose-branch bit node)
  (cond ((= bit 0) (left node))
        ((= bit 1) (right node))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; Sets of weighted elements
;;
;; A set of leaves and trees will be represented as a list of elements,
;; arranged in increasing order of weight.
;; (the tree-generating algorithm requires that we work with sets of
;; leaves and trees, successively merging the two smallest items [weight-wise].)
;;
;; Items are compared by their weights, and the element being added
;; to the set is never already in it.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))


;; The following procedure takes a list of symbol-frequency pairs
;; such as ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves,
;; ready to be merged according to the Huffman algorithm:

(define (make-node-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-node-set (cdr pairs))))))

;; A lookup procedure for sets represented by unordered lists:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


;; Tests
;;
;; Test 1: Decoding
;;
;; (define sample-tree
;;   (merge-nodes (make-leaf 'A 4)
;;                (merge-nodes
;;                 (make-leaf 'B 2)
;;                 (merge-nodes (make-leaf 'D 1)
;;                              (make-leaf 'C 1)))))
;;
;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;
;; Use the decode procedure to decode the message, and give the result.
;;
;; (decode sample-message sample-tree) ; (a d a b b c a)
;;
;; 0 110 0 10 10 111 0
;; a   d a  b  b   c a


;; Test 2: Encoding
;;
;; (define sample-message '(a d a b b c a))
;;
;; (encode sample-message sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)


;; Test 3: Huffman tree generation
;;
;; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;;
;; ((() () (a) 4)
;;  ((() () (b) 2)
;;   ((() () (d) 1)
;;    (() () (c) 1)
;;    (d c) 2)
;;   (b d c) 4)
;;  (a b d c) 8)


;; Test 4: Encode a rock song
;;
;; (define symbol-frequency-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2)
;;                                  (NA 16) (SHA 3) (YIP 9) (WAH 1)))
;;
;; (define huffman-tree-1 (generate-huffman-tree symbol-frequency-pairs))
;;
;; (define message '(Get a job
;;                       Sha na na na na na na na na
;;                       Get a job
;;                       Sha na na na na na na na na
;;                       Wah yip yip yip yip yip yip yip yip yip
;;                       Sha boom))
;;
;; (define encoded (encode message huffman-tree-1))
;;
;; encoded:
;; 
;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0
;;  0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1
;;  1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0
;;  1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;;
;; (length encoded) ; 84
