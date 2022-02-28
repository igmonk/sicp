;; Special Form: lisp-value
;;
;; Each frame in the stream is used to instantiate the variables
;; in the pattern, the indicated predicate is applied, and
;; the frames for which the predicate returns false
;; are filtered out of the input stream.
;;
;; An error results if there are unbound pattern variables.

(load "stream-utils.scm")
(load "instantiator.scm")

(define (install-lisp-value qevaluator)
  (let ((qeval (qevaluator 'qeval))
        (extend-qeval (qevaluator 'extend-qeval)))

    (define (lisp-value call frame-stream)
      (stream-flatmap
       (lambda (frame)
         (if (execute
              (instantiate
               call
               frame
               (lambda (v f)
                 (error "Unknown pat var -- LISP-VALUE" v))))
             (singleton-stream frame)
             the-empty-stream))
       frame-stream))

    (define (predicate exps) (car exps))
    (define (args exps) (cdr exps))

    ;; 'execute' is implemented using 'eval' and 'apply'
    ;; from the underlying Lisp system.
    ;;
    ;; The procedure applies the predicate to the arguments.
    ;; It evals the predicate expression to get the procedure to apply.
    ;; It does not, however, evaluate the arguments, since
    ;; they are already the actual arguments, not expressions,
    ;; whose evaluation (in Lisp) will produce the arguments.
    (define (execute exp)
      (apply (eval (predicate exp) user-initial-environment)
             (args exp)))

    (extend-qeval 'lisp-value lisp-value)))
