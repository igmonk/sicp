;; 2.2 Hierarchical Data and the Closure Property

;; The ability to create pairs whose elements are pairs is the essence of
;; list structure's importance as a representational tool.
;; This property is referred to as the 'closure property' of 'cons'.

;; In general, an operation for combining data objects satisfies the closure property
;; if the results of combining things with that operation can themselves be combined
;; using the same operation.

(load "../../common.scm")


;; Representing Sequences
;;
;; One of the useful structures we can build with pairs is a _sequence_ -
;; an ordered collection of data objects.
;;
;; Representation of a sequence as a chain of pairs constructed by
;; nested 'cons' operations:
;;
;; ('nil' is no longer of Scheme anymore, use '() instead.)

(cons 1
      (cons 2
            (cons 3
                  (cons 4 '())))) ; (1 2 3 4)

;; The above sequence could be produced by:

(list 1 2 3 4) ; (1 2 3 4)

;; (define one-through-four (list 1 2 3 4))

;; (car one-through-four) ; 1
;; (cdr one-through-four) ; (2 3 4)

;; (car (cdr one-through-four)) ; 2

;; (cons 10 one-through-four) ; (10 1 2 3 4)
;; (cons 5 one-through-four)  ; (5 1 2 3 4)

;; List dialects provide abbreviations for nested applications of car and cdr.
;;
;; The names of all such procedures start with c and end with r.
;; Each a between them stands for a car operation and each d for a cdr operation,
;; to be applied in the same order in which they appear in the name.
;;
;; (cadr <arg>) = (car (cdr <arg>))

;; (cadr one-through-four)   ; 2
;; (caddr one-through-four)  ; 3
;; (cadddr one-through-four) ; 4

;; (car (list 1 (list 2 3) 4))    ; 1
;; (cdr (list 1 (list 2 3) 4))    ; ((2 3) 4)
;; (cadr (list 1 (list 1 2) 4))   ; (1 2)
;; (caadr (list 1 (list 2 3) 4))  ; 2
;; (cdadr (list 1 (list 2 3) 4))  ; (3)
;; (cadadr (list 1 (list 2 3) 4)) ; 3


;; List operations

;; 1) The n-th item of a given list
;;
;; The procedure list-ref takes as arguments a list and a number n
;; and returns the n-th item of the list.
;; It is customary to number the elements of the list beginning with 0.

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;; (define squares (list 1 4 9 16 25))
;;
;; (list-ref squares 0) ; 1
;; (list-ref squares 1) ; 4
;; (list-ref squares 2) ; 9
;; (list-ref squares 3) ; 16
;; (list-ref squares 4) ; 25

;; 2) Length
;;
;; Often we cdr down the whole list.
;; To aid in this, Scheme includes a primitive predicate 'null?', which tests
;; whether its argument is the empty list.
;;
;; The procedure 'length', which returns the number of items in a list,
;; illustrates this typical pattern of use:

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;; (define odds (list 1 3 5 7))
;;
;; (length odds) ; 4

;; We can also compute length in an iterative style:

(define (length items)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (+ result 1))))
  (iter items 0))

;; (length odds) ; 4

;; 3) Append
;;
;; This operation takes two lists as arguments and combines their elements
;; to make a new list.
;;
;; Below, 'append' is implemented using a recursive plan.
;; To append lists list1 and list2, do the following:
;; - if list1 is the empty list, then the result is just list2
;; - otherwise, append the cdr of list1 and list2,
;;   and cons the car of list1 onto the result

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; (append squares odds) ; (1 4 9 16 25 1 3 5 7)
;; (append odds squares) ; (1 3 5 7 1 4 9 16 25)


;; Mapping over lists
;;
;; One extremely useful operation is to apply some transformation to each element in a list
;; and generate the list of results.
;;
;; For instance, the following procedure scales each number in a list by a given factor:

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;; (scale-list (list 1 2 3 4 5) 10) ; (10 20 30 40 50)

;; We can abstract this general idea and capture it as a common pattern
;; expressed as a higher-order procedure.
;;
;; The higher-order procedure here is called map.
;; Map takes as arguments a procedure of one argument and a list,
;; and returns a list of the results produced by applying the procedure
;; to each element in the list:

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

;; (map abs (list -10 2.5 -11.6 17)) ; (10 2.5 11.6 17)
;;
;; (map (lambda (x) (* x x))
;;      (list 1 2 3 4))
;;
;; (1 4 9 16)

;; Now we can give a new definition of scale-list in terms of map:

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; Map is an important construct, not only because it captures a common pattern,
;; but because it establishes a higher level of abstraction in dealing with lists.
;;
;; In the original definition of scale-list, the recursive structure of the program
;; draws attention to the element-by-element processing of the list.
;; Defining scale-list in terms of map suppresses that level of detail and emphasizes
;; that scaling transforms a list of elements to a list of results.


;; Hierarchical Structures
;;
;; The representation of sequences in terms of lists generalizes naturally
;; to represent sequences whose elements may themselves be sequences.
;;
;; Another way to think of sequences whose elements are sequences is as trees.
;; The elements of the sequence are the branches of the tree,
;; and elements that are themselves sequences are subtrees.
;;
;; Recursion is a natural tool for dealing with tree structures,
;; since we can often reduce operations on trees to operations on their branches,
;; which reduce in turn to operations on the branches of the branches, and so on,
;; until we reach the leaves of the tree.

;; As an example, compare the length procedure of section 2.2.1
;; with the count-leaves procedure, which returns the total number of leaves of a tree:

(define x (cons (list 1 2) (list 3 4)))

;; (length x)       ; 3
;; (count-leaves x) ; 4

(define xx (list x x))

;; (length xx)       ; 2
;; (count-leaves xx) ; 8

;; To aid in writing recursive procedures on trees,
;; Scheme provides the primitive predicate pair?, which tests whether its argument is a pair.

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


;; Mapping over trees

;; 'map' together with recursion is a powerful abstraction for dealing with trees.
;;
;; The 'scale-tree' procedure, analogous to 'scale-list' of section 2.2.1,
;; takes as arguments a numeric factor and a tree whose leaves are numbers.
;; It returnes a tree of the same shape, where each number is multiplied by the factor.
;;
;; The recursive plan for 'scale-tree' is similar to the one for 'count-leaves':

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; (scale-tree t1 10) ; (10 (20 (30 40) 50) (60 70)

;; Another way to implement 'scale-tree' is to regard the tree as a sequence
;; of sub-trees and use map.
;; We map over the sequence, scaling each sub-tree in turn, and return the list of results.
;; In the base case, where the tree is a leaf, we simply multiply by the factor:

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; (scale-tree t1 10) ; (10 (20 (30 40) 50) (60 70)


;; Sequences as Conventional Interfaces
;;
;; This section introduces another powerful design principle for working with
;; data structures - the use of 'conventional interfaces'.
;;
;; Our ability to formulate operations analogous to higher-order procedures
;; in section 1.3 for working with compound data depends crucially on the style
;; in which we manipulate our data structures.

;; Consider, for example, the following procedure, which takes a tree as argument
;; and computes the sum of the squares of the leaves that are odd:

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

;; (sum-odd-squares t1) ; 84

;; On the surface, 'sum-odd-squares' is very different from the following one,
;; which constructs a list of all the even Fibonacci numbers Fib(k), where
;; k is less than or equal to a given integer n:

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; (even-fibs 10) ; (0 2 8 34)

;; Despite the fact that these two procedures are structurally very different,
;; a more abstract description of the two computations reveals a great deal of similarity.
;;
;; The first program:
;; - enumerates the leaves of a tree;
;; - filters them, selecting the odd ones;
;; - squares each of the selected ones; and
;; - accumulates the results using +, starting with 0.
;;
;; The second program:
;; - enumerates the integers from 0 to n;
;; - computes the Fibonacci number for each integer;
;; - filters them, selecting the even ones; and
;; accumulates the results using cons, starting with the empty list.
;;
;; A signal-processing engineer would find it natural to conceptualize these processes
;; in terms of signals flowing through a cascade of stages, each of which
;; implements part of the program plan.
;;
;; In sum-odd-squares, we begin with an 'enumerator',
;; which generates a 'signal' consisting of the leaves of a given tree.
;; This signal is passed through a 'filter', which eliminates all but the odd elements.
;; The resulting signal is in turn passed through a 'map', which is a 'transducer'
;; that applies the square procedure to each element.
;; The output of the map is then fed to an 'accumulator',
;; which combines the elements using +, starting from an initial 0.
;;
;; The plan for even-fibs is analogous.
;;
;; Our two procedures decompose the computations in a different way,
;; spreading the enumeration over the program and mingling it with the map,
;; the filter, and the accumulation.
;; If we could organize our programs to make the signal-flow structure manifest
;; in the procedures we write, this would increase the conceptual clarity of the resulting code.


;; Sequence Operations
;;
;; The key to organizing programs so as to more clearly reflect the signal-flow structure
;; is to concentrate on the 'signals' that flow from one stage in the process to the next.
;; If we represent these signals as lists, then we can use list operations to implement
;; the processing at each of the stages.

;; 1. The mapping stage.
;; 
;; The mapping stages of the signal-flow diagrams can be implemented
;; using the 'map' procedure.

;; 2. The filtering stage
;;
;; Filtering a sequence to select only those elements that satisfy a given predicate
;; is accomplished by

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; (filter odd? (list 1 2 3 4 5 6 7)) ; (1 3 5 7)

;; 3. Accumulation
;;
;; Accumulations can be implemented by

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Accumulation alternative: iterative process
;; Note: accumulations without the commutative property result in reversed lists.
;;
;; (define (accumulate op initial sequence)
;;   (if (null? sequence)
;;       initial
;;       (accumulate op
;;                   (op initial (car sequence))
;;                   (cdr sequence))))


;; (accumulate + 0 (list 1 2 3 4 5))      ; 15
;; (accumulate * 1 (list 1 2 3 4 5))      ; 120
;; (accumulate cons '() (list 1 2 3 4 5)) ; (1 2 3 4 5)

;; 4. Enumeration
;;
;; All that remains to implement signal-flow diagrams is to enumerate
;; the sequence of elements to be processed.
;;
;; For even-fibs, we need to generate the sequence of integers in a given range:

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; (enumerate-interval 2 7) ; (2 3 4 5 6 7)

;; To enumerate the leaves of a tree, we can use
;; (this is, in fact, precisely the 'fringe' procedure from ex. 2.28)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; (enumerate-tree (list 1 (list 2 (list 3 4)) 5)) ; (1 2 3 4 5)

;; Now we can reformulate sum-odd-squares and even-fibs as in the signal-flow diagrams.
;;
;; For sum-odd-squares, we enumerate the sequence of leaves of the tree,
;; filter this to keep only the odd numbers in the sequence, square each element,
;; and sum the results:

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

;; (sum-odd-squares t1) ; 84

;; For even-fibs, we enumerate the integers from 0 to n,
;; generate the Fibonacci number for each of these integers,
;; filter the resulting sequence to keep only the even elements,
;; and accumulate the results into a list:

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

;; (even-fibs 10) ; (0 2 8 34)


;; Nested Mappings
;;
;; The sequence paradigm can be extended to include many computations that
;; are commonly expressed using nested loops.

;; Consider this problem:
;;
;; Given a positive integer n, find all ordered pairs of distinct positive integers i and j,
;; where 1 <= j < i <= n, such that i + j is prime.
;;
;; A natural way to organize this computation is
;; to generate the sequence of all ordered pairs of positive integers less than or equal to n,
;; filter to select those pairs whose sum is prime, and then,
;; for each pair (i, j) that passes through the filter, produce the triple (i,j,i + j).
;;
;; (accumulate append
;;             '()
;;             (map (lambda (i)
;;                    (map (lambda (j)
;;                           (list i j))
;;                         (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n)))

;; The combination of mapping and accumulating with append is so common
;; in this sort of program that we will isolate it as a separate procedure:

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Now filter this sequence of pairs to find those whose sum is prime.
;; The filter predicate is called for each element of the sequence;
;; its argument is a pair and it must extract the integers from the pair.
;; Thus, the predicate to apply to each element in the sequence is

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; Finally, generate the sequence of results by mapping over the filtered pairs
;; using the following procedure, which constructs a triple consisting of
;; the two elements of the pair along with their sum:

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Combining all these steps yields the complete procedure:

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j)
                         (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


;; (prime-sum-pairs 6)
;;
;; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))


;; Nested mappings are also useful for sequences other than those that enumerate intervals.
;; Suppose we wish to generate all the permutations of a set S;
;; that is, all the ways of ordering the items in the set.
;;
;; For instance, the permutations of {1,2,3} are
;; {1,2,3}, {1,3,2}, {2,1,3}, {2,3,1}, {3,1,2}, and {3,2,1}.
;;
;; Here is a plan for generating the permutations of S:
;; For each item x in S, recursively generate the sequence of permutations of S - x,
;; and adjoin x to the front of each one.
;; This yields, for each x in S, the sequence of permutations of S that begin with x.
;; Combining these sequences for all x gives all the permutations of S:

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x)
            (not (= x item)))
          sequence))

;; (permutations (list 1 2 3))
;;
;; ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))


;; Example: A Picture Language
;;
;; This section presents a simple language for drawing pictures
;; that illustrates the power of data abstraction and closure,
;; and also exploits higher-order procedures in an essential way.
;;
;; The language is designed to make it easy to experiment with patterns,
;; which are composed of repeated elements that are shifted and scaled.
;;
;; In this language, the data objects being combined are represented as procedures
;; rather than as list structure. Just as 'cons', which satisfies the closure property,
;; allowed us to easily build arbitrarily complicated list structure,
;; the operations in this language, which also satisfy the closure property,
;; allow us to easily build arbitrarily complicated patterns.

;; Part of the elegance of this picture language is that there is only one kind of element,
;; called a painter.
;; A painter draws an image that is shifted and scaled to fit within
;; a designated parallelogram-shaped frame.
;;
;; To combine images, we use various operations that construct new painters from given painters.

;; Wishful thinking in action:
;;
;; while defining procedures, we express the concepts that we have in our heads,
;; but the definition of which comes later in the chapter.
;;
;; 'beside' operation takes two painters and produces a new, compound painter that
;; draws the first painter's image in the left half of the frame and
;; the second painter's image in the right half of the frame.
;;
;; 'below' operation takes two painters and produces a compound painter that
;; draws the first painter's image below the second painter's image.
;;
;; 'flip-vert' operation takes a painter and produces a painter that
;; draws its image upside-down.
;;
;; 'flip-horiz' operation takes a painter and produces a painter that
;; draws the original painter's image left-to-right reversed.

;; Once we can combine painters,
;; we would like to be able to abstract typical patterns of combining painters.
;;
;; We will implement the painter operations as Scheme procedures.
;; This means that we don't need a special abstraction mechanism in the picture language:
;; Since the means of combination are ordinary Scheme procedures,
;; we automatically have the capability to do anything with painter operations
;; that we can do with procedures.

;; 'right-split' makes painters split and branch towards the right:

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; 'corner-split' makes painters branch upwards as well as towards the right:

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; By placing four copies of a 'corner-split' appropriately,
;; we obtain a pattern called 'square-limit' (see figure 2.9):

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; Higher-order operations

;; In addition to abstracting patterns of combining painters,
;; we can work at a higher level, abstracting patterns of combining painter operations.
;;
;; That is, we can view the painter operations as elements to manipulate and
;; can write means of combination for these elements -- procedures that
;; take painter operations as arguments and create new painter operations.

;; For example, 'flipped-pairs' and 'square-limit' each arrange four copies
;; of a painter's image in a square pattern; they differ only in how they orient the copies.
;;
;; One way to abstract this pattern of painter combination is with the following procedure,
;; which takes four one-argument painter operations and produces a painter operation that
;; transforms a given painter with those four operations and arranges the results in a square.
;;
;; Tl, tr, bl, and br are the transformations to apply to the top left copy, the top right copy,
;; the bottom left copy, and the bottom right copy, respectively.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; Then 'flipped-pairs' can be defined in terms of 'square-of-four' as follows:

(define (flipped-pairs painter)
  (square-of-four identity flip-vert identity flip-vert))

;; and 'square-limit' can be expressed as

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;; Frames
;; 
;; A frame can be described by three vectors -- an origin vector and two edge vectors.
;; The origin vector specifies the offset of the frame's origin from some absolute
;; origin in the plane, and the edge vectors specify the offsets of the frame's corners
;; from its origin.
;;
;; If the edges are perpendicular, the frame will be rectangular.
;; Otherwise the frame will be a more general parallelogram.

;; In accordance with data abstraction, we need not be specific yet about
;; how frames are represented, other than to say that there is a constructor 'make-frame',
;; which takes three vectors and produces a frame, and
;; three corresponding selectors 'origin-frame', 'edge1-frame', and 'edge2-frame'.

;; We will use coordinates in the unit square (0 <= x,y <= 1) to specify images.
;; With each frame, we associate a 'frame coordinate map', which will be used to
;; shift and scale images to fit the frame.
;;
;; The map transforms the unit square into the frame by mapping
;; the vector v=(x,y) to the vector sum:
;;
;; Origin(Frame) + x * Edge1(Frame) + y * Edge2(frame)
;;
;; For example,
;; - (0,0) is mapped to the origin of the frame,
;; - (1,1) to the vertex diagonally opposite the origin
;; - (0.5,0.5) to the center of the frame.

;; The following procedure creates a frame's coordinate map:

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

;; Applying 'frame-coord-map' to a frame returns a procedure that, given a vector,
;; returns a vector.
;; If the argument vector is in the unit square, the result vector will be in the frame.


;; Painters
;;
;; A painter is represented as a procedure that, given a frame as argument,
;; draws a particular image shifted and scaled to fit the frame.

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; Notice, the frame coordinate map can be initialized once:

(define (segments->painter segment-list)
  (lambda (frame)
    (let ((fcm (frame-coord-map frame)))
      (for-each
       (lambda (segment)
         (draw-line (fcm (start-segment segment))
                    (fcm (end-segment segment))))
       segment-list))))

;; The segments are given using coordinates with respect to the unit square.
;; For each segment in the list, the painter transforms the segment endpoints
;; with the frame coordinate map and draws a line between the transformed points.


;; Transforming and combining painters
;;
;; Painter operations are based on the procedure 'transform-painter',
;; which takes as arguments a painter and information on how to transform a frame
;; and produces a new painter.
;;
;; The transformed painter, when called on a frame, transforms the frame and
;; calls the original painter on the transformed frame.
;;
;; The arguments to 'transform-painter' are points (represented as vectors)
;; that specify the corners of the new frame:
;; when mapped into the frame, the first point specifies the new frame's origin
;; and the other two specify the ends of its edge vectors.
;;
;; Thus, arguments within the unit square specify a frame contained within the original frame.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

;; Here's how to flip painter images vertically:

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; Using transform-painter, we can easily define new transformations.
;; For example, we can define a painter that shrinks its image
;; to the upper-right quarter of the frame it is given:

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

;; Other transformations rotate images counterclockwise by 90 degrees

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; or squash images towards the center of the frame:

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;; Frame transformation is also the key to defining means of combining two or more painters.
;; The 'beside' procedure, for example, takes two painters, transforms them to
;; paint in the left and right halves of an argument frame respectively,
;; and produces a new, compound painter.
;; When the compound painter is given a frame, it calls the first transformed painter
;; to paint in the left half of the frame and calls the second transformed painter
;; to paint in the right half of the frame:

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((left-painter (transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           split-point
                                           (make-vect 0.0 1.0)))
          (right-painter (transform-painter painter2
                                            split-point
                                            (make-vect 1.0 0.0)
                                            (make-vect 0.5 1.0))))
      (lambda (frame)
        (left-painter frame)
        (right-painter frame)))))


;; The punchline (from the corresponding lecture):
;;
;; We've implemented the means of combination themselves as procedures.
;; When we go to abstract in this language, everything that Lisp supplies us
;; for manipulating procedures is automatically available to do things
;; in this picture language.
;;
;; Not only is this language implemented in Lisp, but the language
;; is nicely embedded in Lisp.
;; By embedding the language in this way, all the power of Lisp is
;; automatically available as an extension to whatever you want to do.
;;
;; Lisp is a lousy language for doing any particular problem.
;; What it is good for is figuring out the right language that you want
;; and embedding that in Lisp. That's the real power of this approach to design.
;;
;; What in a system is procedure and what's data?
;; There isn't any difference.


;; Levels of language for robust design

;; We have also obtained a glimpse of another crucial idea about languages and program design.
;; This is the approach of 'stratified design', the notion that a complex system
;; should be structured as a sequence of levels that are described using a sequence of languages.
;;
;; Each level is constructed by combining parts that are regarded as primitive at that level,
;; and the parts constructed at each level are used as primitives at the next level.
;;
;; The language used at each level of a stratified design has primitives, means of combination,
;; and means of abstraction appropriate to that level of detail.

;; Stratified design helps make programs robust, that is,
;; it makes it likely that small changes in a specification will require
;; correspondingly small changes in the program.
;;
;; In general, each level of a stratified design provides
;; a different vocabulary for expressing the characteristics of the system,
;; and a different kind of ability to change it.
