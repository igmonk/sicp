;; Exercise 5.43
;;
;; We argued in section 4.1.6 that internal definitions for
;; block structure should not be considered "real" defines.
;;
;; Rather, a procedure body should be interpreted as if
;; the internal variables being defined were installed as ordinary
;; lambda variables initialized to their correct values using set!.
;;
;; Section 4.1.6 and exercise 4.16 showed how to modify
;; the metacircular interpreter to accomplish this by scanning out
;; internal definitions.
;;
;; Modify the compiler to perform the same transformation before
;; it compiles a procedure body.


;; The following special forms have been added to the compiler:
;; - begin
;; - let
;;
;; to support the syntax transformations required when there are
;; internal definitions in the lambda body.
;;
;; As in exercise 4.16, lambda has been extended with the following
;; procedure:

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


;; The above transformation is performed before the procedure body
;; is compiled:

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
     (compile-sequence (scan-out-defines   ;; Scan out defines
                        (lambda-body exp))
                       'val
                       'return
                       (extend-cenvironment formals cenv)))))


;; See: compile-lambda.scm
;;      ex-4.16.scm
