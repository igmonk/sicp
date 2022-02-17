;; Exercise 4.41
;;
;; Write an ordinary Scheme program to solve
;; the multiple dwelling puzzle.


;; One way of solving the task could be:
;;
;; 1. Generate the sequence of all possible sets of assignments
;; 2. Filter out those sets not conforming to the limitations

;; The most challenging part of the task is to generate
;; a sequence of all the possible sets of assignments
;; in a way that is generic and parameterized.
;;
;; Each set might contain same assignments of floors
;; to different people, making the overall number of
;; assignment sets equal n^n,
;; where n is the number of all possible choices (floors),
;;         that is also equal to the number of people.
;;
;; Those sets that do not conform to the requirements
;; (limitations) get filtered out.


;; 1. Generate the sequence of all possible sets of assignments
;;
;;     n - the number of all possible choices, as well as
;;         the size of each set in the sequence
;; depth - the number indicating the current position in the set:
;;
;;      depth
;; --------->
;; 1 1 1 1 1
;; 1 1 1 1 2
;; 1 1 1 1 3
;; 1 1 1 1 4
;; 1 1 1 1 5
;; 1 1 1 2 1
;; 1 1 1 2 2
;; 1 1 1 2 3
;; ...
;; ...
;; 5 5 5 5 1
;; 5 5 5 5 2
;; 5 5 5 5 3
;; 5 5 5 5 4
;; 5 5 5 5 5
;;
;; Thus, the overal size of the generated sequence is n^n.

(define (explode n)
  (define n-range (integers-until n))
  (define (inner depth)
    (if (< depth n)
        (flatmap (lambda (i)
                   (map (lambda (l) (cons i l))
                        (inner (+ depth 1))))
                 n-range)
        (map list n-range)))
  (inner 1))

;; (length (explode 5)) ; 5^5 = 3125
;; (length (explode 4)) ; 4^4 = 256
;; (length (explode 3)) ; 3^3 = 27
;; (length (explode 2)) ; 2^2 = 4
;; (length (explode 1)) ; 1^1 = 1


;; 2. Filter out those sets not conforming to the limitations
;;
;; As soon as the sequence of all sets of assignmetns have been
;; generated, the task boils down to the filtering those sets
;; that do not conform to the given limitations.


;; To make the code more readable, the following selectors
;; get introduced: 

(define (get-baker l) (car l))
(define (get-cooper l) (cadr l))
(define (get-fletcher l) (caddr l))
(define (get-miller l) (cadddr l))
(define (get-smith l) (cadr (cdddr l)))


;; Next comes the predicate used for filtering:

(define (dwelling-predicate assignment)
  (let ((baker (get-baker assignment))
        (cooper (get-cooper assignment))
        (fletcher (get-fletcher assignment))
        (miller (get-miller assignment))
        (smith (get-smith assignment)))
    (and (distinct?
          (list baker cooper fletcher miller smith))
         (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1)))))


;; And finally, the procedure that generates the set of
;; assignments, filters it out and transforms the result
;; to a readable format:

(define (solve-multiple-dwelling)
  (map (lambda (x)
         (list (list 'baker (get-baker x))
               (list 'cooper (get-cooper x))
               (list 'fletcher (get-fletcher x))
               (list 'miller (get-miller x))
               (list 'smith (get-smith x))))
       (filter dwelling-predicate (explode 5))))

;; (solve-multiple-dwelling)
;;
;; The result is a list of one item:
;;
;; (((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))


;; Utils

(define (integers-until n)
  (define (inner i)
    (if (> i n)
        '()
        (cons i (inner (+ i 1)))))
  (inner 1))

;; (integers-until 5) ; (1 2 3 4 5)


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
