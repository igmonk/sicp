;; Exercise 2.25
;;
;; Give combinations of cars and cdrs that will pick 7 from each of the following lists:
;;
;; 1. (1 3 (5 7) 9)
;; 2. ((7))
;; 3. (1 (2 (3 (4 (5 (6 7))))))


;; 1. (1 3 (5 7) 9)

(define l1 (list 1 3 (list 5 7) 9))

;; (car (cdr (car (cdr (cdr l1))))) ; 7
;; (cadr (caddr l1))                ; 7


;; 2. ((7))

(define l2 (list (list 7)))

;; (car (car l2)) ; 7
;; (caar l2)      ; 7


;; 3. (1 (2 (3 (4 (5 (6 7))))))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))) ; 7
;; (cadr (cadr (cadr (cadr (cadr (cadr l3))))))                               ; 7
;; (cadadr (cadadr (cadadr l3)))                                              ; 7
