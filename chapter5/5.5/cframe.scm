;; Compile-time environment frame

(define (make-empty-cframe) (make-cframe '()))

(define (make-cframe vars)

  (define (empty?) (null? vars))

  (define (find-var-index var)
    (define (inner rest-vars index)
      (cond ((null? rest-vars) false)
            ((eq? (car rest-vars) var) index)
            (else (inner (cdr rest-vars)
                         (+ 1 index)))))
    (inner vars 0))

  (define (dispatch m)
    (cond ((eq? m 'empty?) (empty?))
          ((eq? m 'find-var-index) find-var-index)
          (else (error "Unknown operation -- MAKE-CFRAME" m))))

  dispatch)


;; Syntactic sugar to allow for ordinary procedural
;; syntax use to access the local procedures of objects.

(define (empty-cframe? cframe) (cframe 'empty?))

(define (find-var-index var cframe)
  ((cframe 'find-var-index) var))
