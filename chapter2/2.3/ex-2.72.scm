;; Exercise 2.72
;;
;; Consider the encoding procedure that you designed in exercise 2.68.
;;
;; What is the order of growth in the number of steps needed to encode a symbol?
;; Be sure to include the number of steps needed to search the symbol list
;; at each node encountered.
;;
;; To answer this question in general is difficult.
;; Consider the special case where the relative frequencies of the n symbols
;; are as described in exercise 2.71, and give the order of growth (as a function of n)
;; of the number of steps needed to encode the most frequent and
;; least frequent symbols in the alphabet.


;; The encoding procedure designed in exercise 2.68 goes down the Huffman tree
;; and makes use of the element-of-set? procedure defined for sets
;; represented by unordered lists.
;;
;; For a variable-length code, which a Huffman code is, the number of bits
;; required for a symbol representation ranges from 1 to n, where n is
;; the size of the alphabet and, at the same time, the maximum depth
;; of a Huffman tree.
;;
;; Therefore, the order of growth in the number of steps needed to encode a symbol,
;; excluding the element-of-set? procedure from the equation, is θ(n).
;;
;; Next, the order of growth in the number of steps needed to define whether
;; an element belongs to a set represented by an unordered list is θ(n).
;;
;; Hence, by adding the element-of-set? procedure to the equation,
;; the resulting order of growth in the number of steps of the procedure
;; that encodes a symbol is θ(n^2).

;; Given the special case where the relative frequencies of the n symbols
;; are as described in ex. 2.71, the order of growth of the number of steps
;; needed to encode the most frequent and least frequent symbols
;; in the alphabet are θ(n) and θ(n^2), respectively.
;;
;; The reason behind that is the following:
;; 1) in order to encode the most frequent symbol
;;    we only need to ensure that the root of a Huffman tree contains that symbol
;;    (one invocation of element-of-set?) and return its code leading to the leaf
;;    that represents that symbol.
;; 2) in order to encode the least frequent symbol
;;    we need to go down the tree through its longest path, whose length
;;    is equal to the depth of the tree (n-1), invoking element-of-set? at each level
;;    while constructing the sequence of bits.
