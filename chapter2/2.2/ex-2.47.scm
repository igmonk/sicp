;; Exercise 2.47
;;
;; Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors
;; to produce an implementation for frames.


(load "workbook.scm")

;; 1. list constructor

(define (origin-frame f)
  (list-ref f 0))

(define (edge1-frame f)
  (list-ref f 1))

(define (edge2-frame f)
  (list-ref f 2))


;; (define f1 (make-frame "origin" "edge1" "edge2"))
;;
;; (origin-frame f1) ; "origin"
;; (edge1-frame f1)  ; "edge1"
;; (edge2-frame f1)  ; "edge2"


;; 2. cons constructor

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cddr f))


;; (define f2 (make-frame "origin" "edge1" "edge2"))
;;
;; (origin-frame f2) ; "origin"
;; (edge1-frame f2)  ; "edge1"
;; (edge2-frame f2)  ; "edge2"
