;; 4.3 Variations on a Scheme - Nondeterministic Computing
;;
;; To support a programming paradigm called "nondeterministic computing"
;; the Scheme evaluator is extended by building into it a facility
;; to support automatic search.
;;
;; Nondeterministic computing, like stream processing, is useful for
;; "generate and test" applications.
;;
;; The key idea here is that expressions in a nondeterministic language
;; can have more than one possible value. The nondeterministic program
;; evaluator will work by automatically choosing a possible value and
;; keeping track of the choice.
;;
;; With nondeterministic evaluation, an expression represents
;; the exploration of a set of possible worlds, each determined by
;; a set of choices. Some of the possible worlds lead to dead ends,
;; while others have useful values.


;; 4.3.1 Amb and Search
;;
;; To extend Scheme to support nondeterminism, a new special form
;; is introduced - 'amb'.
;;
;; The expression
;;
;; (amb <e1> <e2> ... <en>)
;;
;; returns the value of one of the n expressions <ei> "ambiguously".
;;
;; amb with a single choice produces an ordinary (single) value.
;;
;; amb with no choices - the expression (amb) - is an expression
;; with no acceptable values.
;;
;; The requirement that a particular predicate expression 'p'
;; must be true can be expressed as follows:

(define (require p)
  (if (not p) (amb)))

;; Automatic search strategies:
;; - chronological backtracking (depth-first search)
;; - dependency-directed backtracking
;; - truth maintenance
;;
;; The amb evaluator developed in this section implementes a search strategy
;; known as 'depth-first search' or 'chronological backtracking'.

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;; Driver loop
;;
;; The driver loop for the amb evaluator has some unusual properties.
;;
;; It reads an expression and prints the value of the first non-failing
;; execution.
;;
;; If we want to see the value of the next successful execution,
;; we can ask the interpreter to backtrack and attempt to generate
;; a second non-failing execution.


;; 4.3.2 Examples of Nondeterministic Programs
;;
;; The advantage of nondeterministic programming is that we can
;; suppress the details of how search is carried out, thereby
;; expressing our programs at a higher level of abstraction.
;;
;; The implementation of the amb evaluator can be used to:
;; - solve logic puzzles
;; - parse natural language
;;   (to match the input against some grammatical structure)


;; Solve logic puzzles
;;
;; The following puzzle (taken from Dinesman 1968) is typical of
;; a large class of simple logic puzzles:
;;
;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors
;; of an apartment house that contains only five floors.
;; Baker does not live on the top floor.
;; Cooper does not live on the bottom floor.
;; Fletcher does not live on either the top or the bottom floor.
;; Miller lives on a higher floor than does Cooper.
;; Smith does not live on a floor adjacent to Fletcher's.
;; Fletcher does not live on a floor adjacent to Cooper's.
;; Where does everyone live?

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; (multiple-dwelling) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; try-again           ; There are no more values


;; Parsing natural language
;;
;; Programs designed to accept natural language as input
;; usually start by attempting to parse the input, that is,
;; to match the input against some grammatical structure.
;;
;; There are a few thing required:
;; - be able to identify the parts of speech of individual words
;; - a grammar - a set of rules describing how grammatical elements
;;   are composed from simpler elements

;; Start off with some lists that classify various words:
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;; A very simple grammar:
;; a sentence is a noun phrase followed by a verb, where
;; a noun phrase consists of an article followed by a noun.
;;
;; "The cat eats" is parsed as follows:
(sentence (nout-phrase (article the) (noun cat))
          (verb eats))

;; To parse a sentence, its two constituent pieces are identified,
;; and a list of these two elements, tagged with the symbol 'sentence',
;; is returned:
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

;; To start the parsing, set *unparsed* to be the entire input,
;; try to parse a sentence, and check that nothing is left over:
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; Try the parser and verify that it works the test sentence:
(parse '(the cat eats))
;; (sentence (noun-phrase (article the) (noun cat)) (verb eats))
try-again
;; There are no more values


;; Grammar extensions.
;;
;; Automatic search and backtracking really pay off, however,
;; when we consider more complex grammars where there are choices
;; for how the units can be decomposed.
;;
;; Extending the grammar with a list of prepositions.

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

;; To permit such things as "a cat in the class".
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


(parse '(the student with the cat sleeps in the class))
;; (sentence
;;  (noun-phrase
;;   (simple-noun-phrase (article the) (noun student))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat))))
;;  (verb-phrase
;;   (verb sleeps)
;;   (prep-phrase (prep in)
;;                (simple-noun-phrase (article the)
;;                                    (noun class)))))

try-again
;; There are no more values


;; A given input may have more than one legal parse.
;;
;; In the sentence
;; "The professor lectures to the student with the cat."
;; it may be that:
;; - the professor is lecturing with the cat, or
;; - the student has the cat
;;
;; The nondeterministic program finds both possibilities.

(parse '(the professor lectures to the student with the cat))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the) (noun student))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat)))))

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (noun student))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (noun cat)))))))

try-again
;; There are no more values


;; 4.3.3 Implementing the Amb Evaluator
;;
;; The evaluation of an ordinary Scheme expression may:
;; - return a value
;; - never terminate
;; - signal an error
;;
;; In nondeterministic Scheme the evaluation of an expression may
;; in addition result in the discovery of a dead end, in which case
;; evaluation must backtrack to a previous choice point.
;;
;; The execution procedures in the amb evaluator take three arguments:
;; - the environment
;; - success continuation
;; - failure continuation
;;
;; See: evaluator.scm
;;      evaluator-test.scm
;;      eval-amb.scm
;;      test-utils.scm
