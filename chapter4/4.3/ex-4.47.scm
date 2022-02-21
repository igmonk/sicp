;; Exercise 4.47
;;
;; Louis Reasoner suggests that, since a verb phrase is either
;; a verb or a verb phrase followed by a prepositional phrase,
;; it would be much more straightforward to define the procedure
;; parse-verb-phrase as follows (and similarly for noun phrases):

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;; Does this work?
;; Does the program's behavior change if we interchange
;; the order of expressions in the amb?


;; 1. Louis's suggestion results in the infinite loop.

;; (parse '(the student with the cat sleeps in the class))

;; The recursive call is issued every time the input does not
;; start with a verb. That happends when the amb evaluator
;; picks up the second choice containing the recursive call,
;; which leads to 2 alternatives to choose from, of which
;; the second one will lead to the same 2 alternatives again,
;; and so on.


;; 2. If the order of expressions in the amb gets interchanged,
;;    the amb evaluator will be recursively and indefinitely
;;    select the first alternative, since its evaluation is
;;    the infinite loop. Under these circumstances the input
;;    will be left unread.
