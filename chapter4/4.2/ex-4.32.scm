;; Exercise 4.32
;;
;; Give some examples that illustrate the difference between
;; the streams of chapter 3 and the "lazier" lazy lists
;; described in this section.
;;
;; How can you take advantage of this extra laziness?


;; The "lazier" lazy lists, where the 'car' of the list is delayed,
;; help avoid problems posed by mutually dependent internal
;; definitions containing loops (3.5.4), for ex. 'integral'.
;;
;; One can take advantage of this extra laziness by building
;; delayed versions of more general kinds of list structures,
;; for ex. "lazy trees" used in Artificial Intelligence.
;;
;; An example of a lazy tree is a game tree, where the nodes are
;; labeled by game positions, so that the children of a node are labeled
;; with the game positions that can be reached in one move from that node.
;; Game trees proved to be useful in such games as tic-tac-toe,
;; chess, and alike.
;;
;; See: Why Functional Programming Matters / J. Hughes
;;      https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
