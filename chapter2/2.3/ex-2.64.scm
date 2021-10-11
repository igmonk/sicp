;; Exercise
;;
;; The following procedure list->tree converts an ordered list to a balanced binary tree.
;;
;; The helper procedure partial-tree takes as arguments an integer n and list of at least
;; n elements and constructs a balanced tree containing the first n elements of the list.
;; The result returned by partial-tree is a pair (formed with cons)
;; whose car is the constructed tree and whose cdr is the list of elements
;; not included in the tree.

(load "workbook.scm")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; (list->tree '(1 3 5 7 9 11)) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))


;; a. Write a short paragraph explaining as clearly as you can how partial-tree works.
;;    Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

;; In light of the lack of index access, or, more precisely, θ(n) access to n-th element,
;; the algorithm suggests keeping track of those elements that are used to build a sub-tree
;; at each recursion call and those left unused.
;; The latter are intended for building the right-subtree each time a recursion call returns.
;;
;; At each non-terminating case, the implementation operates with elements belonging to
;; one of the four categories:
;; 1) elements preceeding the n/2-th position -> becomes the left sub-tree
;; 2) the element at the n/2-th position -> becomes the tree node
;; 3) elements between the n/2-th position (exclusive)
;;    and the n-th position (inclusive) -> becomes the right-sub-tree
;; 4) elements after the n-th position
;;
;;
;; (list->tree '(1 3 5 7 9 11)) returns a tree of the following form:
;;
;;         5
;;        / \
;;       /   \
;;      /     \
;;     1       9
;;      \     / \
;;       3   7   11


;; b. What is the order of growth in the number of steps required by list->tree
;;    to convert a list of n elements?

;; The order of growth is θ(n) since each element of the original list
;; is traversed once.
