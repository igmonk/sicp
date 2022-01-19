;; Sequence utility procedures

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Decouple pairs: iterative

(define (decouple-pairs pairs)
  (define (inner rest l1 l2)
    (if (null? rest)
        (list l1 l2)
        (let ((pair (car rest)))
          (inner (cdr rest)
                 (append l1 (list (car pair)))
                 (append l2 (list (cadr pair)))))))
  (inner pairs '() '()))


;; (define pairs1 '((1 10) (2 20) (3 30) (4 40) (5 50)))
;;
;; (decouple-pairs '())    ; (() ())
;; (decouple-pairs pairs1) ; ((1 2 3 4 5) (10 20 30 40 50))


;; Decouple pairs: recursive
;;
;; (define (decouple-pairs pairs)
;;   (if (null? pairs)
;;       '()
;;       (let ((pair (car pairs))
;;             (decoupled (decouple-pairs (cdr pairs))))
;;         (if (null? decoupled)
;;             (list (list (car pair))
;;                   (list (cadr pair)))
;;             (list (cons (car pair) (car decoupled))
;;                   (cons (cadr pair) (cadr decoupled)))))))
;;
;; (decouple-pairs '())    ; ()
;; (decouple-pairs pairs1) ; ((1 2 3 4 5) (10 20 30 40 50))

;; Note:
;;   The iterative decouple-pairs returns a list consisting of
;;   two empty lists when its argument is the empty list
;;   In contrast, the recursive decouple-pairs returns just
;;   the empty list (with no inner elements).
