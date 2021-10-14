;; 2.3 Symbolic Data

;; In this section the representational capability of the language is extended
;; by introducing the ability to work with arbitrary symbols as data.

;; Using eq?, we can implement a useful procedure called 'memq'.
;;
;; This takes two arguments, a symbol and a list.
;; If the symbol is not contained in the list (i.e., is not eq? to any item in the list),
;; then memq returns false.
;; Otherwise, it returns the sublist of the list beginning with the first
;; occurrence of the symbol:

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; (memq 'apple '(pear banana prune))            ; false
;; (memq 'apple '(x (apple sause) y apple pear)) ; (apple pear)


;; Example: Symbolic Differentiation
;;
;; As an illustration of symbol manipulation and a further illustration of data abstraction,
;; consider the design of a procedure that performs symbolic differentiation of
;; algebraic expressions.
;;
;; We would like the procedure to take as arguments an algebraic expression and
;; a variable and to return the derivative of the expression with respect to the variable.
;;
;; For example, if the arguments to the procedure are
;;
;; ax^2 + bx + c  and  x
;;
;; the procedure should return 2ax + b.

;; In developing the symbolic-differentiation program, we will follow the same strategy
;; of data abstraction that we followed in developing the rational-number system of section 2.1.1.
;;
;; That is, we will first define a differentiation algorithm that operates on abstract objects
;; such as 'sums', 'products', and 'variables' without worrying about
;; how these are to be represented.
;; Only afterward will we address the representation problem.

;; The differentiation program with abstract data
;;
;; In order to keep things simple, we will consider a very simple
;; symbolic-differentiation program that handles expressions that are built up
;; using only the operations of addition and multiplication with two arguments.
;;
;; Differentiation of any such expression can be carried out by
;; applying the following reduction rules:
;;
;; dc/dx = 0  for c a constant or a variable different from x
;; dx/dx = 1
;; d(u+v)/dx = du/dx + dv/dx
;; d(uv)/dx = u(dv/dx) + v(du/dx)
;;
;; The latter two rules are recursive in nature. That is,
;; to obtain the derivative of a sum we first find the derivatives of the terms and add them.

;; To embody these rules in a procedure we indulge in a little wishful thinking,
;; as we did in designing the rational-number implementation.
;;
;; Let us assume that we already have procedures to implement the following selectors,
;; constructors, and predicates:
;;
;; (variable? e)            Is e a variable?
;; (same-variable? v1 v2)   Are v1 and v2 the same variable?
;; (sum? e)                 Is e a sum?
;; (addend e)	            Addend of the sum e.
;; (augend e)	            Augend of the sum e.
;; (make-sum a1 a2)	    Construct the sum of a1 and a2.
;; (product? e)             Is e a product?
;; (multiplier e)	    Multiplier of the product e.
;; (multiplicand e)	    Multiplicand of the product e.
;; (make-product m1 m2)	    Construct the product of m1 and m2.
;;
;; Using these, and the primitive predicate number?, which identifies numbers,
;; we can express the differentiation rules as the following procedure:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; This deriv procedure incorporates the complete differentiation algorithm.
;; Since it is expressed in terms of abstract data, it will work no matter how
;; we choose to represent algebraic expressions, as long as we design
;; a proper set of selectors and constructors.

;; Representing algebraic expressions
;;
;; One especially straightforward way to use list structure to
;; represent algebraic expressions is to use the same parenthesized prefix notation
;; that Lisp uses for combinations; that is, to represent
;;
;; ax + b
;;
;; as
;;
;; (+ (* a x) b)
;;
;; Then our data representation for the differentiation problem is as follows:

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

;; Thus, we need only combine these with the algorithm as embodied by deriv
;; in order to have a working symbolic-differentiation program.

;; (deriv '(+ x 3) 'x) ; (+ 1 0)
;; (deriv '(* x y) 'x) ; (+ (* x 0) (* y 1))
;;
;; (deriv '(* (* x y) (+ x 3)) 'x)
;;
;; (+ (* (* x y) (+ 1 0))
;;    (* (+ x 3)
;;       (+ (* x 0) (* y 1))))

;; The program produces answers that are correct; however, they are unsimplified.
;; As the third example shows, this becomes a serious issue
;; when the expressions are complex.

;; We can adopt a similar to the rational-number reduction strategy here.
;; We won't change deriv at all. Instead, we will change make-sum so that
;; if both summands are numbers, make-sum will add them and return their sum.
;; Also, if one of the summands is 0, then make-sum will return the other summand.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Similarly, we will change make-product to build in the rules that
;; 0 times anything is 0 and 1 times anything is the thing itself:

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;; (deriv '(+ x 3) 'x) ; 1
;; (deriv '(* x y) 'x) ; y
;;
;; (deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* (+ x 3) y))

;; Although this is quite an improvement, the third example shows that
;; there is still a long way to go before we get a program that puts expressions
;; into a form that we might agree is 'simplest'.
;;
;; The problem of algebraic simplification is complex because, among other reasons,
;; a form that may be simplest for one purpose may not be for another.


;; Example: Representing Sets
;;
;; Informally, a set is simply a collection of distinct objects.
;; To give a more precise definition we can employ the method of data abstraction.
;; That is, we define 'set' by specifying the operations
;; that are to be used on sets:
;; - union-set
;;     computes the union of two sets, which is the set
;;     containing each element that appears in either argument
;; - intersection-set
;;     computes the intersection of two sets, which is the set
;;     containing only elements that appear in both arguments
;; - element-of-set?
;;     is a predicate that determines whether a given element is a member of a set
;; - adjoin-set
;;     takes an object and a set as arguments and returns a set
;;     that contains the elements of the original set and also
;;     the adjoined element
;;
;; From the viewpoint of data abstraction, we are free to design any representation
;; that implements these operations in a way consistent with
;; the interpretations given above.
;;
;; More formally, the operations must satisfy a collection of rules:
;; - for any set S and any object x, (element-of-set? x (adjoin-set x S)) is true
;; - for any sets S and T and any object x, (element-of-set? x (union-set S T))
;;   is equal to (or (element-of-set? x S) (element-of-set? x T))
;; - for any object x, (element-of-set? x '()) is false


;; Sets as unordered lists
;;
;; One way to represent a set is as a list of its elements
;; in which no element appears more than once.
;; The empty set is represented by the empty list.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; (element-of-set? 1 '(1 2 3)) ; true
;; (element-of-set? 3 '(1 2 3)) ; true
;; (element-of-set? 5 '(1 2 3)) ; false

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; (adjoin-set 1 '(1 2 3)) ; (1 2 3)
;; (adjoin-set 3 '(1 2 3)) ; (1 2 3)
;; (adjoin-set 5 '(1 2 3)) ; (5 1 2 3)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; (intersection-set '(1 2 3) '(1 2 3)) ; (1 2 3)
;; (intersection-set '(1 2 3) '(2 3 4)) ; (2 3)
;; (intersection-set '(1 2 3) '(3 4 5)) ; (3)
;; (intersection-set '(1 2 3) '(4 5 6)) ; ()

;; In designing a representation, one of the issues we should be concerned with is efficiency.
;; Consider the number of steps required by our set operations.
;;
;; Since they all use element-of-set?, the speed of this operation has a major impact
;; on the efficiency of the set implementation as a whole.
;;
;; Now, in order to check whether an object is a member of a set,
;; element-of-set? may have to scan the entire set.
;; (In the worst case, the object turns out not to be in the set.)
;;
;; Hence, if the set has n elements, element-of-set? might take up to n steps.
;; Thus, the number of steps required grows as θ(n).
;;
;; The number of steps required by adjoin-set, which uses this operation, also grows as θ(n).
;;
;; For intersection-set, which does an element-of-set? check for each element of set1,
;; the number of steps required grows as the product of the sizes of the sets involved,
;; or θ(n^2) for two sets of size n. The same will be true of union-set.


;; Sets as ordered lists
;;
;; One way to speed up our set operations is to change the representation
;; so that the set elements are listed in increasing order.
;;
;; To keep it simple, we will consider only the case where the set elements are numbers,
;; so that we can compare elements using > and <.
;;
;; We will represent a set of numbers by listing its elements in increasing order.

;; One advantage of ordering shows up in element-of-set?:
;; In checking for the presence of an item, we no longer have to scan the entire set.
;; If we reach a set element that is larger than the item we are looking for,
;; then we know that the item is not in the set:

(define (element-of-set? x set)
  (cond ((or (null? set) (< x (car set))) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; (element-of-set? 1 '(1 2 3)) ; true
;; (element-of-set? 3 '(1 2 3)) ; true
;; (element-of-set? 5 '(1 2 3)) ; false

;; How many steps does this save?
;;
;; In the worst case, the item we are looking for may be the largest one in the set,
;; so the number of steps is the same as for the unordered representation.
;;
;; On the other hand, if we search for items of many different sizes
;; we can expect that sometimes we will be able to stop searching at a point
;; near the beginning of the list and that other times we will still need
;; to examine most of the list.
;;
;; On the average we should expect to have to examine about half of the items in the set.
;; Thus, the average number of steps required will be about n/2.
;; This is still θ(n) growth, but it does save us, on the average,
;; a factor of 2 in number of steps over the previous implementation.


;; A more impressive speedup is obtained with intersection-set.
;;
;; Begin by comparing the initial elements, x1 and x2, of the two sets.
;; If x1 equals x2, then that gives an element of the intersection,
;; and the rest of the intersection is the intersection of the cdrs of the two sets.
;;
;; Suppose, however, that x1 is less than x2. Since x2 is the smallest element in set2,
;; we can immediately conclude that x1 cannot appear anywhere in set2 and
;; hence is not in the intersection. Hence, the intersection is equal to
;; the intersection of set2 with the cdr of set1.
;;
;; Similarly, if x2 is less than x1, then the intersection is given by
;; the intersection of set1 with the cdr of set2.

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

;; (intersection-set '(1 2 3) '(1 2 3)) ; (1 2 3)
;; (intersection-set '(1 2 3) '(2 3 4)) ; (2 3)
;; (intersection-set '(1 2 3) '(3 4 5)) ; (3)
;; (intersection-set '(1 2 3) '(4 5 6)) ; ()

;; To estimate the number of steps required by this process,
;; observe that at each step we reduce the intersection problem to
;; computing intersections of smaller sets - removing the first element
;; from set1 or set2 or both.
;;
;; Thus, the number of steps required is at most the sum of the sizes of set1 and set2,
;; rather than the product of the sizes as with the unordered representation.
;;
;; This is θ(n) growth rather than θ(n^2) - a considerable speedup,
;; even for sets of moderate size.


;; Sets as binary trees
;;
;; The set elements are arranged in the form of a tree.
;; Each node of the tree holds one element of the set ('entry'), and
;; a link to each of two other (possibly empty) nodes.
;;
;; The 'left' link points to elements smaller than the one at the node.
;; The 'right' link points to elements greater than the one at the node.
;;
;; The only thing we require for a valid representation is that
;; all elements in the left subtree be smaller than the node entry and
;; that all elements in the right subtree be larger.
;;
;; At each step, the problem of searching a tree of size n is reduced to
;; searching a tree of size n/2 (if the tree is 'balanced').
;; Since the size of the tree is halved at each step, the number of steps
;; needed to search a tree of size n grows as θ(log(n)).

;; We are representing sets in terms of trees, and trees in terms of lists - in effect,
;; a data abstraction built upon a data abstraction.
;;
;; We can regard the procedures entry, left-branch, right-branch, and make-tree as
;; a way of isolating the abstraction of a 'binary tree' from the particular way
;; we might wish to represent such a tree in terms of list structure.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


;; Now we can write the element-of-set? procedure:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; (define t1
;;   (make-tree
;;    7
;;    (make-tree
;;     3
;;     (make-tree 1 '() '())
;;     (make-tree 5 '() '()))
;;    (make-tree
;;     9
;;     '()
;;     (make-tree 11 '() '()))))

;; (element-of-set? 7 t1)  ; true
;; (element-of-set? 1 t1)  ; true
;; (element-of-set? 11 t1) ; true
;; (element-of-set? 12 t1) ; false

;; Adjoining an item to a set also requires θ(log(n)) steps.
;;
;; To adjoin an item x, we compare x with the node entry to determine whether
;; x should be added to the right or to the left branch, and
;; having adjoined x to the appropriate branch we piece this newly constructed branch
;; together with the original entry and the other branch.
;;
;; If x is equal to the entry, we just return the node.
;; If we are asked to adjoin x to an empty tree,
;; we generate a tree that has x as the entry and empty right and left branches.

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; (element-of-set? 0 (adjoin-set 0 t1))     ; true
;; (element-of-set? -10 (adjoin-set -10 t1)) ; true
;; (element-of-set? 10 (adjoin-set 10 t1))   ; true
;; (element-of-set? 100 (adjoin-set 100 t1)) ; true
;; (element-of-set? 7 (adjoin-set 7 t1))     ; true

;; How can we be certain that the trees we construct will be balanced?
;;
;; Even if we start with a balanced tree,
;; adding elements with adjoin-set may produce an unbalanced result.
;;
;; Since the position of a newly adjoined element depends on how the element
;; compares with the items already in the set, we can expect that
;; if we add elements 'randomly' the tree will tend to be balanced on the average.
;; But this is not a guarantee.
;;
;; One way to solve this problem is to define an operation that transforms
;; an arbitrary tree into a balanced tree with the same elements.
;; Then we can perform this transformation after every few adjoin-set operations
;; to keep our set in balance.
;;
;; There are also other ways to solve this problem,
;; most of which involve designing new data structures for which
;; searching and insertion both can be done in θ(log n) steps.
;;
;; See: Cormen, Leiserson, and Rivest 1990.


;; Sets and information retrieval
;;
;; A typical data-management system spends a large amount of time
;; accessing or modifying the data in the records and therefore
;; requires an efficient method for accessing records.
;;
;; This is done by identifying a part of each record to serve as an identifying key.
;; A key can be anything that uniquely identifies the record.
;;
;; Whatever the key is, when we define the record as a data structure
;; we should include a key selector procedure that retrieves the key
;; associated with a given record.

;; To locate the record with a given key we use a procedure lookup,
;; which takes as arguments a key and a data base and which returns
;; the record that has that key, or false if there is no such record.
;;
;; If the set of records is implemented as an unordered list, we could use

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-kay (cdr set-of-records)))))

;; Information-retrieval systems in which records have to be 'randomly accessed'
;; are typically implemented by a tree-based method.
;;
;; In designing such a system the methodology of data abstraction can be a great help.
;; The designer can create an initial implementation using a simple,
;; straightforward representation such as unordered lists.
;; This will be unsuitable for the eventual system, but it can be useful in
;; providing a 'quick and dirty' data base with which to test the rest of the system.
;;
;; Later on, the data representation can be modified to be more sophisticated.
;; If the data base is accessed in terms of abstract selectors and constructors,
;; this change in representation will not require any changes to the rest of the system.


;; Example: Huffman Encoding Trees
;;
;; Fixed-length codes represent each symbol in the message
;; with the same number of bits.
;;
;; Variable-length codes may represent each symbol with
;; different number of bits.
;;
;; In general, if our messages are such that some symbols appear very frequently
;; and some very rarely, we can encode data more efficiently
;; (i.e., using fewer bits per message) if we assign shorter codes
;; to the frequent symbols.

;; One of the difficulties of using a variable-length code is knowing when
;; you have reached the end of a symbol in reading a sequence of zeros and ones.
;;
;; A possible solution is using a special separator code (for ex., Morse code).
;;
;; Another solution is to design the code in such a way the no complete code
;; for any symbol is the beginning (or prefix) of the code for another symbol.
;; Such a code is called a 'prefix code'.

;; One particular scheme for doing this is called the Huffman encoding method,
;; after its discoverer, David Huffman.
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

;; Encoding
;;
;; Given a Huffman tree, we can find the encoding of any symbol
;; by starting at the root and moving down until we reach the leaf
;; that holds the symbol.
;;
;; Each time we move down a left branch we add a 0 to the code,
;; and each time we move down a right branch we add a 1.
;;
;; (We decide which branch to follow by testing to see which branch either is
;; the leaf node for the symbol or contains the symbol in its set.)

;; Decoding
;;
;; To decode a bit sequence using a Huffman tree,
;; we begin at the root and use the successive zeros and ones of the bit sequence
;; to determine whether to move down the left or the right branch.
;;
;; Each time we come to a leaf, we have generated a new symbol in the message,
;; at which point we start over from the root of the tree to find the next symbol.

;; Generating Huffman trees
;;
;; The idea is to arrange the tree so that the symbols with the lowest frequency
;; appear farthest away from the root.
;;
;; Begin with the set of leaf nodes, containing symbols and their frequencies,
;; as determined by the initial data from which the code is to be constructed.
;;
;; Now find two leaves with the lowest weights and merge them to produce a node
;; that has these two nodes as its left and right branches.
;; The weight of the new node is the sum of the two weights.
;; Remove the two leaves from the original set and replace them by this new node.
;;
;; Now continue this process. At each step, merge two nodes with the smallest weights,
;; removing them from the set and replacing them with a node that has these two
;; as its left and right branches.
;;
;; The process stops when there is only one node left, which is the root of the entire tree.
;;
;; The algorithm does not always specify a unique tree,
;; because there may not be unique smallest-weight nodes at each step.
;;
;; Also, the choice of the order in which the two nodes are merged
;; (i.e., which will be the right branch and which will be the left branch) is arbitrary.

;; Representing Huffman trees
;;
;; Leaves of the tree are represented by a list consisting of the symbol 'leaf',
;; the symbol at the leaf, and the weight:

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; A general tree will be a list of a left branch, a right branch,
;; a set of symbols, and a weight.
;;
;; The set of symbols will be simply a list of the symbols,
;; rather than some more sophisticated set representation.
;;
;; When we make a tree by merging two nodes,
;; we obtain the weight of the tree as the sum of the weights of the nodes,
;; and the set of symbols as the union of the sets of symbols for the nodes.

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; If we make a tree in this way, we have the following selectors:

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; The procedures 'symbols' and 'weight' must do something slightly different
;; depending on whether they are called with a leaf or a general tree.
;; These are simple examples of generic procedures
;; (procedures that can handle more than one kind of data).

;; The decoding procedure
;;
;; The decoding procedure takes as arguments a list of zeros and ones,
;; together with a Huffman tree.

(define (decode bits tree)
  (define (decode-1 bits sub-tree)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) sub-tree)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; The procedure decode-1 takes two arguments:
;; 1) the list of remaining bits
;; 2) the current position in the tree
;;
;; It keeps moving down the tree, choosing a left or a right branch
;; according to whether the next bit in the list is a zero or a one.
;;
;; When it reaches a leaf, it returns the symbol at that leaf
;; as the next symbol in the message by cons-ing it onto the result
;; of decoding the rest of the message, starting at the root of the tree.

;; Sets of weighted elements
;;
;; A set of leaves and trees will be represented as a list of elements,
;; arranged in increasing order of weight.
;; (the tree-generating algorithm requires that we work with sets of
;; leaves and trees, successively merging the two smallest items [weight-wise].)

;; The following adjoin-set procedure for constructing sets is similar to
;; the one described in ex. 2.61; however, items are compared by their weights,
;; and the element being added to the set is never already in it.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; The following procedure takes a list of symbol-frequency pairs
;; such as ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves,
;; ready to be merged according to the Huffman algorithm:

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

