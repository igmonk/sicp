;; Exercise 4.45
;;
;; With the grammar given above, the following sentence can be parsed
;; in five different ways:
;;
;; "The professor lectures to the student in the class with the cat."
;;
;; Give the five parses and explain the differences in shades of
;; meaning among them.


(parse '(the professor lectures to the student in the class with the cat))
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;;     (verb lectures)
;;     (prep-phrase (prep to)
;;                  (simple-noun-phrase
;;                   (article the) (noun student))))
;;    (prep-phrase (prep in)
;;                 (simple-noun-phrase
;;                  (article the) (noun class))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat)))))
;;
;;
;; Meaning: The professor is lecturing with the cat.
;;          The professor is in the class, whereas
;;          the student may not be there.

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the) (noun student))))
;;   (prep-phrase (prep in)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (noun class))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (noun cat)))))))
;;
;;
;; Meaning: The professor is in the class, whereas
;;          the student may not be there.
;;          The cat is an attribute of the class.

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (noun-phrase
;;                  (simple-noun-phrase
;;                   (article the) (noun student))
;;                  (prep-phrase (prep in)
;;                               (simple-noun-phrase
;;                                (article the) (noun class))))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat)))))
;;
;;
;; Meaning: The professor is lecturing with the cat.
;;          The student is being lectured.
;;          The professor is in the class, whereas
;;          the student may not be there.

try-again
;; (sentence
;;  (simple-noun-phrase
;;   (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (noun-phrase
;;                  (simple-noun-phrase
;;                   (article the) (noun student))
;;                  (prep-phrase (prep in)
;;                               (simple-noun-phrase
;;                                (article the) (noun class))))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (noun cat)))))))
;;
;;
;; Meaning: The student is in the class, whereas
;;          the professor might not be there.
;;          The student has the cat.

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
;;                 (prep-phrase (prep in)
;;                              (noun-phrase
;;                               (simple-noun-phrase
;;                                (article the) (noun class))
;;                               (prep-phrase (prep with)
;;                                            (simple-noun-phrase
;;                                             (article the) (noun cat)))))))))
;;
;;
;; Meaning: The student is in the class, whereas
;;          the professor might not be there.
;;          The cat is an attribute of the class.

try-again
;; There are no more values
