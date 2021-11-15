;; Exercise 3.12
;;
;; The following procedure for appending lists was introduced in section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;; 'append' forms a new list by successively consing the elements of x onto y.
;;
;; The procedure append! is similar to append, but it is a mutator
;; rather than a constructor. It appends the lists by splicing them together,
;; modifying the final pair of x so that its cdr is now y.
;; (It is an error to call append! with an empty x.)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; Here last-pair is a procedure that returns the last pair in its argument:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;; z       ; (a b c d)
;; (cdr x) ; <response-1> (b)

(define w (append! x y))

;; w       ; (a b c d)
;; (cdr x) ; <response-2> (b c d)

;; What are the missing <response>s?
;; Draw box-and-pointer diagrams to explain your answer.


;; The first response is the result of accessing the original list x
;; that remained the same after the construction of the list z,
;; since the operation used to construct z - append - is incapable of
;; modifying list structure.
;;
;; x -> |x|x|--->|x|/|
;;       |        |
;;       a        b
;;
;; y -> |x|x|--->|x|/|
;;       |        |
;;       c        d
;;
;; z -> |x|x|--->|x|x|--->|x|x|--->|x|/|
;;       |        |        |        |
;;       a        b        c        d


;; The second response shows that the list x has been changed
;; upon the execution of the operation used to construct w - append!,
;; which is a mutator and modifies its first argument.
;;
;;       w
;;       |
;;       ↓                       
;; x -> |x|x|--->|x|x|-----------↓
;;       |        |              |
;;       a        b              |
;;                               ↓
;;                         y -> |x|x|--->|x|/|
;;                               |        |
;;                               c        d
