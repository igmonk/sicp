;; Exercise 2.42
;;
;; The 'eight-queens puzzle' asks how to place eight queens on a chessboard
;; so that no queen is in check from any other
;; (i.e., no two queens are in the same row, column, or diagonal).
;;
;; One way to solve the puzzle is to work across the board, placing a queen in each column.
;; Once we have placed k - 1 queens, we must place the kth queen in a position
;; where it does not check any of the queens already on the board.
;;
;; We can formulate this approach recursively:
;; Assume that we have already generated the sequence of all possible ways
;; to place k - 1 queens in the first k - 1 columns of the board.
;; For each of these ways, generate an extended set of positions
;; by placing a queen in each row of the kth column.
;; Now filter these, keeping only the positions for which the queen in the kth column
;; is safe with respect to the other queens.
;; This produces the sequence of all ways to place k queens in the first k columns.
;; By continuing this process, we will produce not only one solution,
;; but all solutions to the puzzle.


;; We implement this solution as a procedure 'queens', which returns a sequence of
;; all solutions to the problem of placing n queens on an n×n chessboard.
;; 'queens' has an internal procedure 'queen-cols' that returns the sequence of
;; all ways to place queens in the first k columns of the board.

(load "workbook.scm")

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; In this procedure 'rest-of-queens' is a way to place k - 1 queens
;; in the first k - 1 columns,
;; and 'new-row' is a proposed row in which to place the queen for the kth column.
;;
;; Complete the program by implementing the representation for sets of board positions,
;; including the procedure 'adjoin-position', which adjoins a new row-column position
;; to a set of positions, and 'empty-board', which represents an empty set of positions.
;;
;; You must also write the procedure 'safe?', which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the others.
;; (Note that we need only check whether the new queen is safe -
;; the other queens are already guaranteed safe with respect to each other.)


(define empty-board '())

;; Below are the constructor and selectors for the data structure
;; denoting a position on the chessboard:

(define (make-position row col)
  (cons row col))

(define (position-row pos)
  (car pos))

(define (position-col pos)
  (cdr pos))

;; The logic of adjoin-position is to create a new position with given row and col,
;; and prepend it to the list of already existing positions.

(define (adjoin-position row col positions)
  (cons (make-position row col) positions))

;; The logic behind determining the position of the new queen in the list
;; conforms to the strategy defined in the adjoin-position procedure.
;; (An alternative approach is appending a new queen to the list of existing positions,
;; and fetching it using the nth procedure.)
;;
;; As soon as the new queen is found, it is time to check whether
;; it's been safely placed on the board with respect to the others.
;;
;; For that, the algorithm enumerates over the list of the rest queens (map),
;; and for each of them checks whether it attacks the new queen (attack?),
;; combining the results in a list of boolean values.
;;
;; If the list of boolean values contains at least one 'true' element,
;; the suggested positions of the queens is considered unsafe.
;;
;; Therefore, the list of booleans is filtered with 'identity' predicate
;; and checked for emptiness.

(define (safe? col positions)
  (let ((new-queen (car positions))    ;; conforms to the adjoin-position's logic
        (rest-queens (cdr positions))) ;; conforms to the adjoin-position's logic
    (null? (filter identity
                   (map (lambda (q)
                          (attacks? new-queen q))
                        rest-queens)))))

(define (attacks? q1 q2)
  (not (safe-queen-pair? q1 q2)))

(define (safe-queen-pair? q1 q2)
  (let ((q1-row (position-row q1))
        (q1-col (position-col q1))
        (q2-row (position-row q2))
        (q2-col (position-col q2)))
    (and (not (= q1-row q2-row))              ;; row check
         (not (= q1-col q2-col))              ;; col check
         (not (= (abs (- q1-row q2-row))      ;; diagonal check
                 (abs (- q1-col q2-col)))))))

;; (length (queens 1)) ; 1
;; (length (queens 2)) ; 0
;; (length (queens 3)) ; 0
;; (length (queens 4)) ; 2
;; (length (queens 5)) ; 10
;; (length (queens 6)) ; 4
;; (length (queens 7)) ; 40
;; (length (queens 8)) ; 92
