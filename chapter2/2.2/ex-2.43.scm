;; Exercise 2.43
;;
;; Louis Reasoner is having a terrible time doing exercise 2.42.
;; His queens procedure seems to work, but it runs extremely slowly.
;; (Louis never does manage to wait long enough for it to solve even the 6× 6 case.)
;;
;; When Louis asks Eva Lu Ator for help, she points out that he has interchanged
;; the order of the nested mappings in the flatmap, writing it as
;;
;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))
;;
;; Explain why this interchange makes the program run slowly.
;; Estimate how long it will take Louis's program to solve the eight-queens puzzle,
;; assuming that the program in exercise 2.42 solves the puzzle in time T.


;; The number of recursive calls to 'queen-cols' made by the original implementation
;; is equal to the size of the chessboard + 1.
;; Therefore, its order of growth is θ(n).

;; Louis's implementation generates a tree-recursive process,
;; where each non-terminal node has the size of the chessboard + 1 children.
;; The depth of the tree is also equal to the size of the chessboard + 1.
;; Thus, the number of recursive calls to 'queen-cols' growth proportionally to
;; the square of the size of the input.
;; Therefore, the order of growth is θ(n^2).

;; Hence, if the original implementation solves the eight-queens puzzle in time T,
;; Louis's implementation solves it in time T^2.
