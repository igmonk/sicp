;; Exercise 4.38
;;
;; Modify the multiple-dwelling procedure to omit the requirement
;; that Smith and Fletcher do not live on adjacent floors.
;; How many solutions are there to this modified puzzle?


;; The original puzzle (section 4.3.2):
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


;; The requirement that Smith and Fletcher do not live on adjacent
;; floor is omitted:

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
;;    (require (not (= (abs (- smith fletcher)) 1))) ; omitted
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; (multiple-dwelling) ; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;; try-again           ; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;; try-again           ; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;; try-again           ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; try-again           ; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
;; try-again           ; There are no more values

;; The number of solutions is 5.


;; Requires: amb evaluator & its driver loop
;;           distinct?
;;           require
;;
;; See: test-utils.scm
