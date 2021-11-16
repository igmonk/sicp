;; Exercise 3.16
;;
;; Ben Bitdiddle decides to write a procedure to count the number
;; of pairs in any list structure.
;;
;; "It's easy," he reasons. "The number of pairs in any structure
;; is the number in the car plus the number in the cdr plus
;; one more to count the current pair."
;;
;; So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct.
;;
;; In particular, draw box-and-pointer diagrams representing list structures
;; made up of exactly three pairs for which Ben's procedure would return 3;
;; return 4; return 7; never return at all.


;; Start with a simple 3-pair structure (s3 in the diagram below),
;; where the car of each non-terminal pair points to the next pair,
;; and the cdr of each pair is nil.
;;
;; As expected, count-pairs returns 3 for this structure.
;;
;; Next, start exploring ways to make count-pairs, which traverses
;; a given structure by recursively going throught the car and cdr of each pair,
;; return something different from 3 by rearranging links in the original structure.
;;
;; For that, one can make use of the empty cdr parts of each pair.
;;
;; The diagram below shows a few possible ways of changing the initial structure (s3)
;; while keeping the total amount of pairs in order to get a different result
;; upon the invocation of count-pairs.
;;
;; Notice, as soon as a circular reference is introduced, the structure stops being
;; traversable.


;;    s3 -> |x|/|     s4 -> |x|/|     s5 -> |x|x|     s7 -> |x|x|     s-inf -> |x|/|
;;           |               |               | |             | |                |
;;           |               |               |_↓             |_↓                |_____
;;           |               |               |               |                  |     ↑
;;           ↓               ↓               ↓               ↓                  ↓     |
;;          |x|/|           |x|x|           |x|/|           |x|x|              |x|/|  |
;;           |               | |             |               | |                |     |
;;           |               |_↓             |               |_↓                |     |
;;           |               |               |               |                  |     |
;;           ↓               ↓               ↓               ↓                  ↓     |
;;          |x|/|           |x|/|           |x|/|           |x|/|              |x|x|--↑
;;           |               |               |               |                  |
;;           |               |               |               |                  |
;;           |               |               |               |                  |
;;           ↓               ↓               ↓               ↓                  ↓
;;           a               a               a               a                  a
;;
;;
;; Count:    3               4               5               7                 Inf


;; A procedure that is to be used for generating a structure
;; of the original type (s3 in the diagram above):

(define (generate-s)
  (cons (cons (cons 'a '()) '()) '()))


;; s3 (same as the original one)
;; count-pairs: 3

(define s3 (generate-s))

;; s3               ; (((a)))
;; (count-pairs s3) ; 3


;; s4
;; count-pairs: 4

(define s4 (generate-s))

(set-cdr! (car s4) (caar s4))

;; s4               ; (((a) a))
;; (count-pairs s4) ; 4


;; s5
;; count-pairs: 5

(define s5 (generate-s))

(set-cdr! s5 (car s5))

;; s5               ; (((a)) (a))
;; (count-pairs s5) ; 5


;; s7
;; count-pairs: 7

(define s7 (generate-s))

(set-cdr! (car s7) (caar s7))
(set-cdr! s7 (car s7))

;; s7               ; (((a) a) (a) a)
;; (count-pairs s7) ; 7


;; s-inf
;; count-pairs: Indefinite

(define s-inf (generate-s))

(set-cdr! (caar s-inf) (car s-inf))

;; s-inf               ; (#0=((a . #0#)))
;; (count-pairs s-inf) ; ;Aborting!: maximum recursion depth exceeded
