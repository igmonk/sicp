;; Exercise 5.1
;;
;; Design a register machine to compute factorials using
;; the iterative algorithm specified by the following procedure.

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Draw data-path and controller diagrams for this machine.


;; The data-path diagram
;;
;;                                                  _
;;                                                 / \
;;                                                / 1 \
;;                                               /_____\
;;                                                  |
;;         ┌----------------------┌-----------------|
;;         |                      |                 |
;;         X p<-1                 X c<-1            |
;;         ↓                      ↓                 |
;; ┌-------------┐       ┌----------------┐         |
;; |   product   |       |     counter    |---------|---------→( > )
;; └-------------┘       └----------------┘         |            ↑
;;   ↑         |           |  ↑         |           |            |
;;   X p<-p*c  |           |  X c<-c+1  |           |            |
;;   |         └---┐   ┌---┘  |         └---┐   ┌---┘            |
;;   |          ___↓___↓___   |          ___↓___↓___             |
;;   |          \         /   |          \         /          ┌-----┐
;;   |           \   *   /    |           \   +   /           |  n  |
;;   |            \_____/     |            \_____/            └-----┘
;;   |               |        |               |
;;   └---------------┘        └---------------┘


;; The controller diagram
;;
;;       start
;;         |
;;         ↓
;;     ┌--------┐
;;     |  p<-1  |
;;     └--------┘
;;         |
;;         ↓
;;     ┌--------┐
;;     |  c<-1  |
;;     └--------┘
;;         |
;;         ↓
;;       /   \  yes
;; ┌---→   >   -----→ done
;; |     \   /
;; |       | no
;; |       ↓
;; |   ┌--------┐
;; |   | p<-p*c |
;; |   └--------┘
;; |       |
;; |       ↓
;; |   ┌--------┐
;; └---| c<-c+1 |
;;     └--------┘
