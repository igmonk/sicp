;; Exercise 2.29
;;
;; A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight
;; or another binary mobile.
;;
;; We can represent a binary mobile using compound data by constructing it
;; from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a 'length' (which must be a number) together with
;; a 'structure', which may be either a number (representing a simple weight)
;; or another mobile:

(define (make-branch length structure)
  (list length structure))


;; a. Write the corresponding selectors 'left-branch' and 'right-branch',
;;    which return the branches of a mobile, and 'branch-length' and 'branch-structure',
;;    which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define lb1 (make-branch 10 100))
(define rb1 (make-branch 5 200))
(define m1 (make-mobile lb1 rb1))
(define m1m1 (make-mobile (make-branch 3 m1)
                          (make-branch 3 m1)))

;; (left-branch m1)       ; (10 100)
;; (right-branch m1)      ; (5 200)
;; (branch-length lb1)    ; 10
;; (branch-structure rb1) ; 200

;; (left-branch m1m1)  ; (3 ((10 100) (5 200)))
;; (right-branch m1m1) ; (3 ((10 100) (5 200)))

;; (branch-length (left-branch m1m1))  ; 3
;; (branch-length (right-branch m1m1)) ; 3


;; b. Using your selectors, define a procedure 'total-weight'
;;    that returns the total weight of a mobile.

;; It is worth noting, the logical representation of a binary mobile is different from
;; its physical structure.
;;
;; In the physical structure the length of a branch is a characreristic of the rode that
;; attaches the branch to its parent node, making both nodes and rodes first-class citizens
;; of the structure.
;; On the contrary, the logical representation only supplies us with
;; tree nodes, of which left is allocated to keep the value of the length of a rode,
;; whereas the right one is intended for keeping the structure of a mobile's branch.
;;
;; Therefore, computing the total weight of a binary mobile requires us to traverse
;; the whole structure of the mobile and reduce it to a number representing the sum of
;; its right leaves (pertaining to a leaf's weight).


;; Alternative 1: even-odd-reduce
;;
;; Reducing a binary mobile can be expressed in terms of another, more general,
;; procedure 'even-odd-reduce', that is recursive by its nature and
;; takes as its arguments:
;; - a list/tree that is to be reduced
;; - a null-value for the base case when the list/tree is empty
;; - a function of one argument to be applied to a leaf
;; - a set of functions to be applied at even/odd levels:
;;   - reducer
;;   - left node selector
;;   - right node selector
;;
;; 'even-odd-reduce' is based on the even/odd property of the depth of a current non-leaf node
;; and handles the reduction of it accordingly.
;;
;; Below is the implementation of the general procedure described above:

(define (even-odd-reduce x null-value leaf-fn even-fn even-left even-right odd-fn odd-left odd-right)
  (define (inner x depth)
    (cond ((null? x) null-value)
          ((not (pair? x)) (leaf-fn x))
          ((even? depth)
           (even-fn (inner (even-left x) (inc depth))
                    (inner (even-right x) (inc depth))))
          (else (odd-fn (inner (odd-left x) (inc depth))
                        (inner (odd-right x) (inc depth))))))
  (inner x 0))

;; Hence, 'deep-reduce-mobile' can be expressed in terms of the general method above:

(define (deep-reduce-mobile mobile null-value leaf-fn branch-fn mobile-fn)
  (even-odd-reduce mobile
                   null-value
                   leaf-fn
                   mobile-fn
                   left-branch
                   right-branch
                   branch-fn
                   branch-length
                   branch-structure))


;; Alternative 2: Mutually recursive reduction
;;
;; Alternatively, a pair of mutually recursive procedures can be created:
;; one that applies a reduction step to a node which happens to be mobile,
;; and another that reduces a node that is a branch.
;;
;; That way, there is no longer need for information about the depth at which
;; reduction is currently being carried out.
;;
;; In addition, both deep-reduce-mobile expose the same interface, which makes
;; their use interchangeable.
;;
;; Among other parameters, the procedures take as arguments:
;; - a function that reduces a node which is a branch (whose left and right children are
;;   the length and structure respectively)
;; - a function that reduces a node which is a structure (either a simple weight or
;;   another mobile, whose left and right children are mobile branches)

(define (deep-reduce-mobile mobile null-value leaf-fn branch-fn mobile-fn)
  (define (reduce-node x reduce-fn child-reduce-fn left-fn right-fn)
    (cond ((null? x) null-value)
          ((not (pair? x)) (leaf-fn x))
          (else (reduce-fn (child-reduce-fn (left-fn x))
                           (child-reduce-fn (right-fn x))))))
  (define (reduce-mobile m)
    (reduce-node m mobile-fn reduce-branch left-branch right-branch))
  (define (reduce-branch b)
    (reduce-node b branch-fn reduce-mobile branch-length branch-structure))
  (reduce-mobile mobile))


;; Given the definition of 'deep-reduce-mobile', 'total-weight' can be defined
;; with the help of a pair of procedures: branch and mobile reducers.
;;
;; The goal of the procedure that reduces a branch is to take its right child
;; into account, while ignoring the left one. The right child is either the simple
;; or total (already reduced) weight of the branch.
;;
;; The goal of the procedure that reduces a structure is to sum up the weights of
;; its branches, which by the moment of reduction have already been accumulated.

(load "../../common.scm")

(define (total-weight mobile)
  (deep-reduce-mobile mobile
                      0
                      identity
                      (lambda (left right) right)
                      (lambda (left right) (+ left right))))

;; (total-weight m1)   ; 300
;; (total-weight m1m1) ; 600


;; c. A mobile is said to be 'balanced' if the torque applied by its top-left branch
;;    is equal to that applied by its top-right branch (that is, if the length of
;;    the left rod multiplied by the weight hanging from that rod is equal to
;;    the corresponding product for the right side) and if each of the submobiles
;;    hanging off its branches is balanced.
;;
;;    Design a predicate that tests whether a binary mobile is balanced.


;; Given the definition of 'total-weight', the torque of a given branch
;; can be found by multiplying the length of the rod from which it hangs
;; by the total weight of the branch structure.

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

;; (torque (left-branch m1))  ; 1000
;; (torque (right-branch m1)) ; 1000

;; (torque (left-branch m1m1))  ; 900
;; (torque (right-branch m1m1)) ; 900


;; Given the definition of 'torque', a predicate that tests whether a binary mobile
;; is balanced can be defined with the help of a pair of branch and mobile reducers.
;;
;; Branch reducer is responsible for creating a new branch from the left and right
;; components, which are the length and reduced structure of the new branch.
;;
;; Mobile reducer takes two branches that belong to the reducible mobile and
;; compares their torque. If their torque is the same, the sum of their weights
;; is returned. Otherwise, the result is zero, which, multiplied by the length later,
;; will propagate upwards until it reaches the root node.
;;
;; Finally, 'deep-reduce-mobile' returns either the total weight of the mobile or zero,
;; which means the mobile is balanced or not, respectively.

(define (deep-balanced? mobile)
  (define (inner mobile)
    (deep-reduce-mobile mobile
                        true
                        identity
                        (lambda (left right) (make-branch left right))
                        (lambda (left right)
                          (if (= (torque left) (torque right))
                              (+ (branch-structure left)
                                 (branch-structure right))
                              0))))
  (not (= (inner mobile) 0)))


;; Test: balanced top-left and top-right branches.
;; Expected result: true
;;
;; (deep-balanced? m1)   ; 300 => true


;; Test: balanced top-left and top-right branches & balanced submobiles.
;; Expected result: true
;;
;; (deep-balanced? m1m1) ; 600 => true


;; Test: balanced top-left and top-right branches & unbalanced submobiles.
;; Expected result: false.

(define lb2 (make-branch 1
                         (make-mobile (make-branch 5 10)
                                      (make-branch 3 10))))

(define rb2 (make-branch 1
                         (make-mobile (make-branch 3 10)
                                      (make-branch 5 10))))

(define m2 (make-mobile lb2 rb2))

;; (deep-balanced? m2) ; 0 => false


;; Test: balanced submobiles & unbalanced top-left and top-right branches.
;; Expected result: false.

(define lb3 (make-branch 1
                         (make-mobile (make-branch 5 10)
                                      (make-branch 2 25))))

(define rb3 (make-branch 1
                         (make-mobile (make-branch 3 10)
                                      (make-branch 2 15))))

(define m3 (make-mobile lb3 rb3))

;; (deep-balanced? m3) ; 0 => false


;; d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to the new representation?

;; The only part that needs to be changed is the selectors for
;; - the mobile right branch
;; - the branch structure


(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

;; Both constructors and selectors belong to the same level of abstraction,
;; answerable for establishing an abstraction barrier between the representation
;; of a data structure and the programs that use it.
;;
;; The rest of the code, which operates at the levels above that abstraction barrier,
;; remains unchanged.
