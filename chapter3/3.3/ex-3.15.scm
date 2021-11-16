;; Exercise 3.15
;;
;; Draw box-and-pointer diagrams to explain the effect of set-to-wow!
;; on the structures z1 and z2 above.


;; The following definitions are taken from the corresponding section:

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; z1 ; ((a b) a b)
;; z2 ; ((a b) a b)

;; (set-to-wow! z1) ; ((wow b) wow b)
;; (set-to-wow! z2) ; ((wow b) a b)

;; x ; (wow b)


;; z1 and z2 after invoking set-to-wow! on them:
;;
;; z1 -> |x|x|
;;        | |
;;        |_↓
;;        |
;;        ↓
;; x -> |x|x|--->|x|/|
;;       |        |
;;      wow       b
;;
;;
;; z2 -> |x|x|--->|x|x|--->|x|/|
;;        |        |        |
;;        |        a        b
;;        |                 |
;;         `----->|x|x|--->|x|/|
;;                 |
;;                wow
