;; Arithmetic Polynomial Package

(load "../../common.scm")
(load "../export-defs.scm")
(load "./arith_lib.scm")

(define (install-polynomial-package)
  ;; internal procedures
  (define (add x y) (apply-generic 'add x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (=zero? x) (apply-generic '=zero? x))
  
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1))
                 (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms l1 (rest-terms l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1)
                                (rest-terms l2)))))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-items (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  
  (define (mul-term-by-all-items t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-items t1 (rest-terms l))))))

  (define (zero-poly? p)
    (define (zero-terms? terms)
      (or (empty-termlist? terms)
          (and (=zero? (coeff (first-term terms)))
               (zero-terms? (rest-terms terms)))))
    (zero-terms? (term-list p)))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))

  (define (neg-terms l)
    (define (neg-term t)
      (make-term (order t)
                 (neg (coeff t))))
    (map neg-term l))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-terms-result (div-terms (term-list p1)
                                           (term-list p2))))
          (let ((quotient-term-list (car div-terms-result))
                (remainder-term-list (cadr div-terms-result)))
            (list (make-poly (variable p1) quotient-term-list)
                  (make-poly (variable p1) remainder-term-list))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  
  (define (div-terms l1 l2)
    (if (empty-termlist? l1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term l1))
              (t2 (first-term l2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) l1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-t (make-term new-o new-c)))
                  (let ((rest-of-result
                         (div-terms
                          (add-terms l1
                                     (neg-terms (mul-terms (list new-t) l2)))
                          l2)))
                    (list (add-terms (list new-t)
                                     (car rest-of-result))
                          (cadr rest-of-result)))))))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (list (export-def 'add '(polynomial polynomial)
                    (lambda (p1 p2) (tag (add-poly p1 p2))))
        (export-def 'mul '(polynomial polynomial)
                    (lambda (p1 p2) (tag (mul-poly p1 p2))))
        (export-def '=zero? '(polynomial) zero-poly?)
        (export-def 'neg '(polynomial)
                    (lambda (p) (tag (neg-poly p))))
        (export-def 'sub '(polynomial polynomial)
                    (lambda (p1 p2) (tag (sub-poly p1 p2))))
        (export-def 'div '(polynomial polynomial)
                    (lambda (p1 p2)
                      (let ((div-result (div-poly p1 p2)))
                        (list (tag (car div-result))
                              (tag (cadr div-result))))))
        (export-def 'make 'polynomial
                    (lambda (var terms) (tag (make-poly var terms))))))
