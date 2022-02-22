;; Exercise 4.48
;;
;; Extend the grammar given above to handle more complex sentences.
;;
;; For example, you could extend noun phrases and verb phrases
;; to include adjectives and adverbs, or you could handle
;; compound sentences.


;; 1. Extend the grammar with a list of adjectives.

(define adjectives '(adjective cheerful sleepy hungry))

(define (parse-simple-noun-phrase)
  (amb  (list 'simple-noun-phrase
              (parse-word articles)
              (parse-word nouns))
        (list 'simple-noun-phrase
              (parse-word articles)
              (parse-word adjectives)
              (parse-word nouns))))

(parse '(the cheerful professor lectures
             to the sleepy student
             with the hungry cat))
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (adjective cheerful) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the) (adjective sleepy) (noun student))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (adjective hungry) (noun cat)))))

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (adjective cheerful) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (adjective sleepy) (noun student))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (adjective hungry) (noun cat)))))))

try-again ;; There are no more values


;; 2. Extend the grammar with a list of adverbs.

(define adverbs '(adverb vigorously enthusiastically aggressively))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb-phrase)))

(define (parse-simple-verb-phrase)
  (amb (list 'simple-verb-phrase
             (parse-word verbs))
       (list 'simple-verb-phrase
             (parse-word adverbs)
             (parse-word verbs))
       (list 'simple-verb-phrase
             (parse-word verbs)
             (parse-word adverbs))))


(parse '(the cheerful professor vigorously lectures
             to the sleepy student
             with the hungry cat))
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (adjective cheerful) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (simple-verb-phrase (adverb vigorously) (verb lectures))
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the) (adjective sleepy) (noun student))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (adjective hungry) (noun cat)))))

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (adjective cheerful) (noun professor))
;;  (verb-phrase
;;   (simple-verb-phrase
;;    (adverb vigorously) (verb lectures))
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (adjective sleepy) (noun student))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (adjective hungry) (noun cat)))))))

try-again ;; There are no more values
