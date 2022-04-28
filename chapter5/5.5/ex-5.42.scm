;; Exercise 5.42
;;
;; Using find-variable from exercise 5.41, rewrite compile-variable
;; and compile-assignment to output lexical-address instructions.
;;
;; In cases where find-variable returns not-found (that is, where
;; the variable is not in the compile-time environment), you should
;; have the code generators use the evaluator operations, as before,
;; to search for the binding.
;;
;; (The only place a variable that is not found at compile time
;; can be is in the global environment, which is part of the run-time
;; environment but is not part of the compile-time environment. [47]
;; Thus, if you wish, you may have the evaluator operations look
;; directly in the global environment, which can be obtained with
;; the operation (op get-global-environment), instead of having them
;; search the whole run-time environment found in env.)
;;
;; Test the modified compiler on a few simple cases, such as
;; the nested lambda combination at the beginning of this section.
;;
;; [47] Lexical addresses cannot be used to access variables
;;      in the global environment, because these names can be defined
;;      and redefined interactively at any time.
;;      With internal definitions scanned out, as in exercise 5.43,
;;      the only definitions the compiler sees are those at top level,
;;      which act on the global environment.
;;      Compilation of a definition does not cause the defined name
;;      to be entered in the compile-time environment.


;; Add the following procedures to the register machine operations:
;;
;; - lexical-addr-lookup
;; - lexical-addr-set!


;; New compile-variable (see compiler.scm):

(define (compile-variable exp target linkage cenv)
  (let ((lexaddr (find-variable exp cenv)))
    (end-with-linkage
     linkage
     (make-instruction-sequence
      '(env) (list target)
      (if (eq? 'not-found lexaddr)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))
          `((assign ,target
                    (op lexical-addr-lookup)
                    (const ,lexaddr)
                    (reg env))))))))


;; New compile-assignment (see compile-assignment.scm):

(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next cenv)))
    (let ((lexaddr (find-variable exp cenv)))
      (end-with-linkage
       linkage
       (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
         '(env val) (list target)
         (if (eq? 'not-found lexaddr)
             `((perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env))
               (assign ,target (const ok)))
             `((perform (op lexical-addr-set!)
                        (const ,lexaddr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))
