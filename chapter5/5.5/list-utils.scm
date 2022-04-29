;; Sequence utility procedures

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;; Set operations

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

;; Set difference: A\B={x:x in A and x not in B}. 
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else
         (cons (car s1)
               (list-difference (cdr s1) s2)))))


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
