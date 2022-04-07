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
