;; Exercise 4.6
;;
;; 'let' expressions are derived expressions, because
;;
;; (let ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)
;;
;; is equivalent to
;;
;; ((lambda (<var1> ... <varn>)
;;    <body>)
;;  <exp1>
;;  ...
;;  <expn>)
;;
;; Implement a syntactic transformation let->combination that
;; reduces evaluating 'let' expressions to evaluating combinations
;; of the type shown above, and add the appropriate clause
;; to 'eval' to handle 'let' expressions.


;; See: eval-let.scm
;;      evaluator-tests.scm
