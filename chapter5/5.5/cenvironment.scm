;; Compile-time Environment
;;
;; A data structure that keeps track of which variables
;; are at which positions in which frames.

(load "cframe.scm")
(load "lexical-address.scm")

(define (make-cenvironment)
  (make-cenvironment-with (make-empty-cframe) '()))

(define (make-cenvironment-with cframe base-cenv)

  (define not-found 'not-found)

  (define (not-found? x) (eq? not-found x))

  (define (empty?)
    (and (empty-cframe? cframe)
         (eq? base-cenv '())))

  (define (extend vars)
    (make-cenvironment-with
     (make-cframe vars)
     dispatch))

  (define (find-var var)
    (if (empty?)
        not-found
        (let ((displacement (find-var-index var cframe)))
          (if displacement
              (make-lexical-addr 0 displacement)
              (let ((lexical-addr (find-variable var base-cenv)))
                (if (not-found? lexical-addr)
                    not-found
                    (make-lexical-addr
                     (+ 1 (lexical-addr-frame-num lexical-addr))
                     (lexical-addr-displacement lexical-addr))))))))

  (define (dispatch m)
    (cond ((eq? m 'empty?) (empty?))
          ((eq? m 'extend) extend)
          ((eq? m 'find-var) find-var)
          (else (error "Unknown operation -- MAKE-CENVIRONMENT" m))))

  dispatch)


;; Syntactic sugar to allow for ordinary procedural
;; syntax use to access the local procedures of objects.

(define (extend-cenvironment vars cenv)
  ((cenv 'extend) vars))

(define (find-variable var cenv)
  ((cenv 'find-var) var))
