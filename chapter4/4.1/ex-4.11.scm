;; Exercise 4.11
;;
;; Instead of representing a frame as a pair of lists,
;; we can represent a frame as a list of bindings, where
;; each binding is a name-value pair.
;;
;; Rewrite the environment operations to use this alternative
;; representation.


;; Since an environment frame was not an abstraction of its own,
;; its internals were scattered across several places in the
;; environment code.
;;
;; Hence, it has been decided to put abstraction barriers
;; between frames (and their bindings) and environments.
;;
;; See: frame.scm
;;      frame-binding.scm
;;      environment.scm
