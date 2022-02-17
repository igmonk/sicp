;; Exercise 4.39
;;
;; Does the order of the restrictions in the multiple-dwelling
;; procedure affect the answer?
;;
;; Does it affect the time to find an answer?
;;
;; If you think it matters, demonstrate a faster program
;; obtained from the given one by reordering the restrictions.
;;
;; If you think it does not matter, argue your case.


;; The order of the restrictions does not affect the answer,
;; since eventually, all the possible choices will be examined
;; to find the answer(s) that comply to all of the restrictions.

;; The order of the restrictions might affect the time
;; to find an answer, since it dictates the order of paths
;; taken to explore the graph of choices.


;; The following enhancements can be suggested:
;; 
;; 1. Since there are 5 amb expressions, we could try to
;;    place on top those restrictions pertained to the 1st amb,
;;    followed by those related to the 2nd amb, and so on.
;;    The problem, however, is that we can't rely on the order
;;    of these amb expressions get evaluated inside the let block.
;;    Hence, here this suggestion is not followed.
;;
;; 2. Since the amb evaluator chooses the first alternative
;;    at each choice point, we can try moving up those restrictions
;;    that work with smaller numbers (corresponding to lower floors).
;;
;; 3. In addition, the heavy tasks (like distinct?) may be moved
;;    down as much as possible. By doing so, the number of times
;;    these tasks need to be run significantly reduces.

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 1)))
    (require (not (= baker 5)))
    (require (not (= fletcher 5)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; (multiple-dwelling) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; try-again           ; There are no more values
