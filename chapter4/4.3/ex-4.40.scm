;; Exercise 4.40
;;
;; In the multiple dwelling problem, how many sets of assignments
;; are there of people to floors, both before and after the requirement
;; that floor assignments be distinct?
;;
;; It is very inefficient to generate all possible assignments
;; of people to floors and then leave it to backtracking to eliminate them.
;;
;; For example, most of the restrictions depend on only one or two of
;; the person-floor variables, and can thus be imposed before floors
;; have been selected for all the people.
;;
;; Write and demonstrate a much more efficient nondeterministic procedure
;; that solves this problem based upon generating only those possibilities
;; that are not already ruled out by previous restrictions.
;;
;; (Hint: This will require a nest of let expressions.)


;; The improvement process is 2-step (with an optional 3rd step):
;;
;; 1. Eliminate the assignments for those choices that are
;;    known for sure beforehand.
;; 2. Locate the requirements right after all the choices
;;    participating in their definitions have been defined.
;;
;; 3. (Optional) Eliminate the assignments for those choices
;;    that have already been used in earlier assignments.
;;    Requires filtering out choices and application of amb.


;; 1. Eliminate the assignments for those choices that are
;;    known for sure beforehand. 

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5))
        (fletcher (amb 2 3 4))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
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

;; Executes noticeable faster than the original.
;;
;; (multiple-dwelling) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; try-again           ; There are no more values


;; 2. Locate the requirements right after all the choices
;;    participating in their definitions have been defined.

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))        
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;; As fast as a rocket.
;;
;; (multiple-dwelling) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; try-again           ; There are no more values


;; 3. (Optional) Eliminate the assignments for those choices
;;    that have already been used in earlier assignments.
;;    Requires filtering out choices and application of amb.
;;
;; This step requires implementing an 'apply' counterpart
;; for the amb evaluator.
;;
;; Then, if 2 is the current choice for cooper, miller can
;; only get one of 1, 3, 4 and 5, and so on.
