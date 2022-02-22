;; Exercise 4.49
;;
;; Alyssa P. Hacker is more interested in generating interesting
;; sentences than in parsing them.
;;
;; She reasons that by simply changing the procedure parse-word
;; so that it ignores the "input sentence" and instead always
;; succeeds and generates an appropriate word, we can use
;; the programs we had built for parsing to do generation instead.
;;
;; Implement Alyssa's idea, and show the first half-dozen or so
;; sentences generated.


;; Below are described two possible solutions.
;;
;; The complexity of resulting sentences depends on
;; the maturity of the parsing methods used to
;; parse (generate) a phrase.


;; 1. Employ amb to enumerate over the given list of words.

(define (parse-word word-list)
  (define (inner part-of-speech words)
    (require (not (null? words)))
    (amb (list part-of-speech (car words))
         (inner part-of-speech (cdr words))))
  (inner (car word-list) (cdr word-list)))

(parse '())
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun cat))
;;  (simple-verb-phrase (verb sleeps)))
;;
;; => the cat sleeps

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun cat))
;;  (verb-phrase
;;   (simple-verb-phrase (verb sleeps))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article a) (noun professor)))))
;;
;; => the cat sleeps with a professor

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun cat))
;;  (verb-phrase
;;   (verb-phrase
;;    (simple-verb-phrase (verb sleeps))
;;    (prep-phrase (prep with)
;;                 (simple-noun-phrase
;;                  (article a) (noun professor))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun professor)))))
;;
;; => the cat sleeps with a professor with the professor

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun cat))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;;     (simple-verb-phrase (verb sleeps))
;;     (prep-phrase (prep with)
;;                  (simple-noun-phrase
;;                   (article a) (noun professor))))
;;    (prep-phrase (prep with)
;;                 (simple-noun-phrase
;;                  (article the) (noun professor))))
;;   (prep-phrase (prep in)
;;                (simple-noun-phrase
;;                 (article the) (noun professor)))))
;;
;; the cat sleeps with a professor with the professor in the professor


;; It can be seen, however, the process enters recursion and takes
;; the first choices to generate subsequent clauses, in accordance with
;; the selected procedure parse-sentence.


;; 2. Take a word at random
;;    Requires random, length, list-ref, abs, min, max to be defined.

(define (parse-word word-list)
  (list (car word-list)
        (take-a-word-at-random (cdr word-list))))

(define (take-a-word-at-random words)
  (list-ref words
            (random-in-range 0 (length words))))

(define (random-in-range low high)
  (let ((range (abs (- high low))))
    (+ (min low high) (random range))))


(parse '())
;; the student studies

try-again
;; the student studies with the student for the student

try-again
;; the student studies with the student for the student by a class


;; Here, a sentence grows by incorporating words randomly selected
;; from the list pertained to their part of speech.

;; By calling 'parse' sequentially (skipping try-again), one can
;; generate a few unrelated sentences, albeit simple (conforming to
;; the first choice of what's been selected by parse-sentence):
;;
;; the class lectures
;; the professor lectures
;; a professor studies
;; etc.
