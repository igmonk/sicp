;; Exercise 4.42
;;
;; Solve the following "Liars" puzzle (from Phillips 1934):
;;
;; Five schoolgirls sat for an examination.
;; Their parents - so they thought - showed an undue degree of interest
;; in the result.
;; They therefore agreed that, in writing home about the examination,
;; each girl should make one true statement and one untrue one.
;; The following are the relevant passages from their letters:
;;
;; - Betty: "Kitty was second in the examination. I was only third."
;; - Ethel: "You'll be glad to hear that I was on top. Joan was second."
;; - Joan: "I was third, and poor old Ethel was bottom."
;; - Kitty: "I came out second. Mary was only fourth."
;; - Mary: "I was fourth. Top place was taken by Betty."
;;
;; What in fact was the order in which the five girls were placed?


;; The following solution is to be executed in the driver loop
;; of the amb evaluator that supports the special form 'xor'.
;; Requires 'require' and distinct? to be defined.

(define (schoolgirls)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;; (schoolgirls) ; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
;; try-again     ; There are no more values
;;
;; See: eval-xor.scm
