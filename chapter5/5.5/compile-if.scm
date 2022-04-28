;; Compile If
;;
;; The code for an 'if' expression compiled with a given target
;; and linkage has the form
;;
;;  <compilation of predicate, target val, linkage next>
;;  (test (op false?) (reg val))
;;  (branch (label false-branch))
;; true-branch
;;  <compilation of consequent with given target and given linkage or after-if>
;; false-branch
;;  <compilation of alternative with given target and linkage>
;; after-if

(load "instruction-seq.scm")
(load "instruction-comb.scm")

(define (install-compile-if compiler)
  (let ((compile (compiler 'compile))
        (make-label (compiler 'make-label))
        (extend-compile (compiler 'extend-compile)))
    
    (define (compile-if exp target linkage cenv)
      (let ((t-branch (make-label 'true-branch))
            (f-branch (make-label 'false-branch))
            (after-if (make-label 'after-if)))
        (let ((consequent-linkage
               (if (eq? linkage 'next) after-if linkage)))
          (let ((p-code (compile (if-predicate exp) 'val 'next cenv))
                (c-code
                 (compile (if-consequent exp) target consequent-linkage cenv))
                (a-code
                 (compile (if-alternative exp) target linkage cenv)))
            (preserving
             '(env continue)
             p-code
             (append-instruction-sequences
              (make-instruction-sequence
               '(val) '()
               `((test (op false?) (reg val))
                 (branch (label ,f-branch))))
              (parallel-instruction-sequences
               (append-instruction-sequences t-branch c-code)
               (append-instruction-sequences f-branch a-code))
              after-if))))))

    (define (if-predicate exp) (cadr exp))
    (define (if-consequent exp) (caddr exp))
    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (cadddr exp)
          'false))

    (extend-compile 'if compile-if)))
