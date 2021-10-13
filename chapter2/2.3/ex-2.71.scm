;; Exercise 2.71
;;
;; Suppose we have a Huffman tree for an alphabet of n symbols,
;; and that the relative frequencies of the symbols are 1, 2, 4, ..., 2^n-1.
;;
;; Sketch the tree for n = 5; for n = 10.
;;
;; In such a tree (for general n) how many bits are required to encode
;; the most frequent symbol? the least frequent symbol?


;; Sketch of the tree for n = 5
;;
;;                                   ((e d c b a) 31)
;;                                  /                \
;;                       ((e d c b) 15)              (a 16)
;;                      /              \
;;             ((e d c) 7)             (b 8)
;;            /           \
;;     ((e d) 3)          (c 4)
;;    /         \
;; (e 1)        (d 2)
;;
;; (generate-huffman-tree '((e 1) (d 2) (c 4) (b 8) (a 16)))
;;
;; (((((leaf e 1)
;;     (leaf d 2)
;;     (e d) 3)
;;    (leaf c 4)
;;    (e d c) 7)
;;   (leaf b 8)
;;   (e d c b) 15)
;;  (leaf a 16)
;;  (e d c b a) 31)


;; Sketch of the tree for n = 10
;;
;;                                              ((j i h g f e d c b a) 1023)
;;                                                           /        \
;;                                       ((j i h g f e d c b) 511)    (a 512)
;;                                                  /        \
;;                                ((j i h g f e d c) 255)    (b 256)
;;                                         /        \
;;                         ((j i h g f e d) 127)    (c 128)
;;                                  /      \
;;                    ((j i h g f e) 63)   (d 64)
;;                           /      \
;;               ((j i h g f) 31)   (e 32)
;;                    /      \
;;          ((j i h g) 15)   (f 16)
;;               /    \
;;       ((j i h) 7)  (g 8)
;;          /    \
;;    ((j i) 3)  (h 4)
;;      /  \
;; (j 1)   (i 2)
;;
;; (generate-huffman-tree '((j 1) (i 2) (h 4) (g 8) (f 16)
;;                          (e 32) (d 64) (c 128) (b 256) (a 512)))
;;
;; ((((((((((leaf j 1)
;;          (leaf i 2)
;;          (j i) 3)
;;         (leaf h 4)
;;         (j i h) 7)
;;        (leaf g 8)
;;        (j i h g) 15)
;;       (leaf f 16)
;;       (j i h g f) 31)
;;      (leaf e 32)
;;      (j i h g f e) 63)
;;     (leaf d 64)
;;     (j i h g f e d) 127)
;;    (leaf c 128)
;;    (j i h g f e d c) 255)
;;   (leaf b 256)
;;   (j i h g f e d c b) 511)
;;  (leaf a 512)
;;  (j i h g f e d c b a) 1023)


;; Each successive merge generates a node whose weight is 1 less than that
;; of the next item in the set (arranged in increasing order of weight).
;;
;; Hence, the resulting tree resembles a list - an unbalanced tree from
;; the data structure perspective, althought from the weights point of view
;; the tree is balanced. 
;;
;; In such a tree (for general n):
;;     1 bit is required to encode the most frequent symbol,
;;   n-1 bits are required to encode the least frequent symbol
