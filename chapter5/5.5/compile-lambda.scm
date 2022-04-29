;; Compile Lambda
;;
;; The object code for a 'lambda' expression must have the form
;;
;; <construct procedure object and assign it to target register>
;; <linkage>
;;
;; When the 'lambda' expression is compiled, the code for
;; the procedure body is also generated.
;;
;; Although the body won't be executed at the time of procedure
;; construction, it is convenient to insert it into the object
;; code right after the code for the lambda.
;;
;; The object code thus has the form
;;
;;  <construct procedure object and assign it to target register>
;;  <code for given linkage> or (goto (label after-lambda))
;;  <compilation of procedure body>
;; after-lambda

(load "instruction-seq.scm")
(load "instruction-comb.scm")
(load "cenvironment.scm")

(define (install-compile-lambda compiler)
  (let ((compile (compiler 'compile))
        (compile-sequence (compiler 'compile-sequence))
        (make-label (compiler 'make-label))
        (extend-compile (compiler 'extend-compile))
        (end-with-linkage (compiler 'end-with-linkage))
        (def-constructor (compiler 'def-constructor))
        (get-constructor (compiler 'get-constructor))
        (get-syntax (compiler 'get-syntax)))

    (define (compile-lambda exp target linkage cenv)
      (let ((proc-entry (make-label 'entry))
            (after-lambda (make-label 'after-lambda)))
        (let ((lambda-linkage
               (if (eq? linkage 'next) after-lambda linkage)))
          (append-instruction-sequences
           (tack-on-instruction-sequence
            (end-with-linkage
             lambda-linkage
             (make-instruction-sequence
              '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
            (compile-lambda-body exp proc-entry cenv))
           after-lambda))))

    (define (compile-lambda-body exp proc-entry cenv)
      (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
         (make-instruction-sequence
          '(env proc argl) '(env)
          `(,proc-entry
            (assign env (op compiled-procedure-env) (reg proc))
            (assign env
                    (op extend-environment)
                    (const ,formals)
                    (reg argl)
                    (reg env))))
         (compile-sequence (scan-out-defines
                            (lambda-body exp))
                           'val
                           'return
                           (extend-cenvironment formals cenv)))))

    ;; Scans the procedure body with internal definitions
    ;;
    ;; (lambda <vars>
    ;;   (define u <e1>)
    ;;   (define v <e2>)
    ;;   <e3>)
    ;;
    ;; and returns a let-equivalent without internal definitions
    ;;
    ;; (lambda <vars>
    ;;   (let ((u '*unassigned*)
    ;;         (v '*unassigned*))
    ;;     (set! u <e1>)
    ;;     (set! v <e2>)
    ;;     <e3>))
    (define (scan-out-defines proc-body)
      (let ((defines (filter define? proc-body)))
        (if (null? defines)
            proc-body
            (let ((other-exps (remove define? proc-body))
                  (def-vars (map (get-syntax 'definition-variable) defines))
                  (def-vals (map (get-syntax 'definition-value) defines)))
              (list ; list since it still is a procedure body (sequence)
               (make-let
                (map var->unassigned-pair def-vars)
                (append (map var-val->make-set! def-vars def-vals)
                        other-exps)))))))

    (define (define? exp) (tagged-list? exp 'define))

    (define (var->unassigned-pair var)
      (list var (quote '*unassigned*)))

    (define (var-val->make-set! var val)
      (make-set! var (list val)))

    ;; Dependency constructors
    (define (make-let . args)
      (apply (get-constructor 'make-let) args))
    (define (make-set! . args)
      (apply (get-constructor 'make-set!) args))

    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))

    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    (extend-compile 'lambda compile-lambda)
    (def-constructor 'make-lambda make-lambda)))
