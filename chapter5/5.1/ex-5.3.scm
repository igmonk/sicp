;; Exercise 5.3
;;
;; Design a machine to compute square roots using Newton's method,
;; as described in section 1.1.7:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Begin by assuming that good-enough? and improve operations
;; are available as primitives.
;;
;; Then show how to expand these in terms of arithmetic operations.
;;
;; Describe each version of the sqrt machine design by drawing
;; a data-path diagram and writing a controller definition
;; in the register-machine language.


;; 1. good-enough? and improve are primitives
;;
;; 1.1 The data-path diagram
;;
;;       _
;;      / \
;;     /   \
;;    / 1.0 \
;;   /_______\
;;       |
;;       |
;;       X
;;       ↓
;; ┌-----------┐
;; |   guess   |---------→( good-enough? )
;; └-----------┘                ↑
;;   ↑       |                  |
;;   X       |               ┌-----┐
;;   |       |               |  x  |
;;   |       |               └-----┘
;;   |       └---┐     ┌--------┘
;;   |        ___↓_____↓___
;;   |        \           /        
;;   |         \ improve /
;;   |          \_______/
;;   |              |
;;   └--------------┘
;;
;;
;; 1.2 The controller definition

(controller
 (assign guess (const 1.0))
 test-guess
   (test (op good-enough?) (reg guess) (reg x))
   (branch (label sqrt-done))
   (assign guess (op improve) (reg guess) (reg x))
   (goto (label test-guess))
 sqrt-done)


;; 2. good-enough? and improve are expanded in terms of
;;    arithmetic operations
;;
;; 2.1 The data-path diagram
;;
;;       _
;;      / \
;;     /   \
;;    / 1.0 \
;;   /_______\
;;       |
;;       |
;;       X
;;       ↓
;; ┌-----------┐                               ┌-----┐
;; |   guess   |                               |  x  |
;; └-----------┘                               └-----┘
;;   ↑      |                                     |
;;   X      |      ┌------------------------------|
;;   |      |      |                              |
;;   |      |--------------------------┐          |
;;   |      |      |                   |          |
;;   |      |-----------┐              |          |
;;   |      |      |    |              |          |
;;   |      |   ___↓____↓___     ______↓______    |
;;   |      |   \          /     \           /    |
;;   |      |    \    /   /       \  square /     |
;;   |      |     \______/         \_______/      |
;;   |      |         |                |          |
;;   |      |     ┌---┘                └------┐   |
;;   |   ___↓_____↓___                     ___↓___↓___
;;   |   \           /                     \         /
;;   |    \ average /                       \   -   /
;;   |     \_______/                         \_____/
;;   |         |                                |
;;   └---------┘                                |
;;                                         _____↓_____
;;                                         \         /
;;                                          \  abs  /
;;                                           \_____/
;;                                              |
;;                                              └---------→( < )
;;                                                           ↑
;;                                                           |
;;                                                          / \
;;                                                         /   \
;;                                                        /     \
;;                                                       / 0.001 \
;;                                                      /_________\
;;
;;
;; * an auxiliary register (t) is not depicted in the diagram.
;;
;; 2.2 The controller definition

(controller
 (assign guess (const 1.0))
 test-guess
   (assign t (op square) (reg guess))
   (assign t (op -) (reg t) (reg x))
   (assign t (op abs) (reg t))
   (test (op <) (reg t) (const 0.001))
   (branch (label sqrt-done))
   (assign t (op /) (reg x) (reg guess))
   (assign guess (op average) (reg guess) (reg t))
   (goto (label test-guess))
 sqrt-done)
