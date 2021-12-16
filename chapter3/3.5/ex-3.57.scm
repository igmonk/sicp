;; Exercise 3.57
;;
;; How many additions are performed when we compute the nth
;; Fibonacci number using the definition of fibs based on
;; the add-streams procedure?
;;
;; Show that the number of additions would be exponentially greater
;; if we had implemented (delay <exp>) simply as (lambda () <exp>),
;; without using the optimization provided by the memo-proc procedure
;; described in section 3.5.1.


;; Since each subsequent element of the generated stream
;; is memoized, it takes n-1 additions to compute the nth
;; Fibonacci number.
;;
;; Had the memo-proc procedure not been used by (delay <exp>),
;; each subsequent element of the resulting sequence would have
;; caused recomputing the pair of immediately preceeding elements,
;; each of which would, in turn, have caused the same recomputing
;; recursively and independently of each other, and so on.
;;
;; Therefore, the process to compute the nth Fibonacci number
;; would have had θ(n^2) order of growth.
