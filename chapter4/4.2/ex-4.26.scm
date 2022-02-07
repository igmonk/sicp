;; Exercise 4.26
;;
;; Ben Bitdiddle and Alyssa P. Hacker disagree over the importance
;; of lazy evaluation for implementing things such as 'unless'.
;;
;; Ben points out that it's possible to implement 'unless' in
;; applicative order as a special form.
;;
;; Alyssa counters that, if one did that, 'unless' would be
;; merely syntax, not a procedure that could be used
;; in conjunction with higher-order procedures.
;;
;; Fill in the details on both sides of the argument.
;;
;; Show how to implement 'unless' as a derived expression
;; (like 'cond' or 'let'), and give an example of a situation where
;; it might be useful to have 'unless' available as a procedure,
;; rather than as a special form.


;; Both Ben and Alyssa are right in their statements:
;; it's possible to implement 'unless' in applicative order
;; as a special form and it would be merely syntax,
;; not a procedure that could be used in conjunctions with
;; higher-order procedures. No objections about any of these.
;;
;; The importance of lazy evaluation for implementing things
;; such as 'unless' is debatable though, since this special form,
;; as well as many others (if, cond, begin, while, etc.),
;; are supposed to control the evaluation flow rather than
;; simply compute something.
;;
;; Hence, it is not evident where such a special form would be
;; useful as a procedure that could be used in conjunction with
;; higher-order procedures.
;;
;; See: eval-unless.scm
;;      evaluator-tests.scm
