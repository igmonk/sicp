;; Exercise 3.51
;;
;; In order to take a closer look at delayed evaluation,
;; we will use the following procedure, which simply returns
;; its argument after printing it:

(load "workbook.scm")
(load "ex-3.50.scm")

(define (show x)
  (display-line x)
  x)

;; What does the interpreter print in response to evaluating
;; each expression in the following sequence?

(define x (stream-map show (stream-enumerate-interval 0 10)))

;; 0
;; Value: x
;;
;; > The procedure 'show' has only been invoked once with
;;   the first item of the resulting stream.


(stream-ref x 5)

;; 1
;; 2
;; 3
;; 4
;; 5
;; Value: 5
;;
;; > In order to get to the 5th element of the stream,
;;   all the previous elements must be forced/realized,
;;   with one small exception: 0 has already been forced
;;   (by evaluating the previous expression).

(stream-ref x 7)

;; 6
;; 7
;; > Same as previous one:
;;   in order to get to the 7th element of the stream,
;;   all the previous ones must be forced/realized.
;;   Since every element to the left of the 6th one
;;   has already been forced, there were only
;;   2 more elements to be forced: 6th and 7th.


;; Reset the environment:
;;
;; (ge (make-top-level-environment))
