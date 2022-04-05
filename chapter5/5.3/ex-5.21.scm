;; Exercise 5.21
;;
;; Implement register machines for the following procedures.
;;
;; Assume that the list-structure memory operations
;; are available as machine primitives.
;;
;; a. Recursive count-leaves:

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; b. Recursive count-leaves with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))


;; Start with generating and testing a few trees

(define t0 '())
(define t2 (cons 1 2))
(define t4 (cons (cons 1 2) (cons 3 4)))
(define t8 (cons (cons (cons 1 2) (cons 3 4))
                 (cons (cons 5 6) (cons 7 8))))

;; (count-leaves t0) ; 0
;; (count-leaves t2) ; 2
;; (count-leaves t4) ; 4
;; (count-leaves t8) ; 8


;; cd to sicp/chapter5/5.2

(load "machine.scm")
(load "basic-machine-ext.scm")


;; a. Recursive count-leaves

(define count-leaves-machine
  (make-machine
   '(tree val tmp continue)
   (list (list 'null? null?) (list 'not not)
         (list 'pair? pair?) (list '+ +)
         (list 'car car) (list 'cdr cdr))
   '((assign continue (label done))
     loop
     (test (op null?) (reg tree))
     (branch (label base-case-null))
     (assign tmp (op pair?) (reg tree)) ; tmp now contains (pair? tree)
     (test (op not) (reg tmp))
     (branch (label base-case-not-pair))
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label after-car))
     (goto (label loop))
     after-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (save val)
     (goto (label loop))
     after-cdr          ; upon return, val contains (count-leaves (cdr tree))
     (restore tmp)      ; tmp now contains (count-leaves (car tree))
     (restore continue)
     (assign val (op +) (reg tmp) (reg val))
     (goto (reg continue))
     base-case-null
     (assign val (const 0))
     (goto (reg continue))
     base-case-not-pair
     (assign val (const 1))
     (goto (reg continue))
     done)))

;; Notice, the register tmp is used in two different usecases:
;; 1) it stores the intermediate value of (pair? tree) to aid
;;    with the complex test (not (pair? tree))
;; 2) it is assigned the value of (count-leaves (car tree))
;;    upon the execution of 'after-cdr'


;; Test: t0
(set-register-contents!
 count-leaves-machine 'tree t0)                   ; done
(start count-leaves-machine)                      ; done
(get-register-contents count-leaves-machine 'val) ; 0

;; Test: t2
(set-register-contents!
 count-leaves-machine 'tree t2)                   ; done
(start count-leaves-machine)                      ; done
(get-register-contents count-leaves-machine 'val) ; 2

;; Test: t4
(set-register-contents!
 count-leaves-machine 'tree t4)                   ; done
(start count-leaves-machine)                      ; done
(get-register-contents count-leaves-machine 'val) ; 4

;; Test: t8
(set-register-contents!
 count-leaves-machine 'tree t8)                   ; done
(start count-leaves-machine)                      ; done
(get-register-contents count-leaves-machine 'val) ; 8


;; b. Recursive count-leaves with explicit counter

(define count-leaves-n-machine
  (make-machine
   '(tree n tmp continue)
   (list (list 'null? null?) (list 'not not)
         (list 'pair? pair?) (list '+ +)
         (list 'car car) (list 'cdr cdr))
   '((assign continue (label done))
     (assign n (const 0))
     loop
     (test (op null?) (reg tree))
     (branch (label base-case-null))
     (assign tmp (op pair?) (reg tree))
     (test (op not) (reg tmp))
     (branch (label base-case-not-pair))
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label after-car))
     (goto (label loop))
     after-car
     (restore tree)
     (restore continue)
     (assign tree (op cdr) (reg tree))
     (goto (label loop))
     base-case-null
     (goto (reg continue))
     base-case-not-pair
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))
     done)))

;; Notice, the process is partially iterative, since after
;; the computation of (car tree) has been done (recursively),
;; the computation of (cdr tree) is done iteratively.
;;
;; Hence, there is no need in the instruction sequence labelled
;; 'after-cdr' as opposed to the first recursive implementation.
;;
;; In addition, the additional register 'val' can be get rid of,
;; since the value of 'n' is not needed after the computation of
;; (car tree) has been done, as opposed to the value of 'tree',
;; which we need to compute the value of (cdr tree).


;; Test: t0
(set-register-contents!
 count-leaves-n-machine 'tree t0)                 ; done
(start count-leaves-n-machine)                    ; done
(get-register-contents count-leaves-n-machine 'n) ; 0

;; Test: t2
(set-register-contents!
 count-leaves-n-machine 'tree t2)                 ; done
(start count-leaves-n-machine)                    ; done
(get-register-contents count-leaves-n-machine 'n) ; 2

;; Test: t4
(set-register-contents!
 count-leaves-n-machine 'tree t4)                 ; done
(start count-leaves-n-machine)                    ; done
(get-register-contents count-leaves-n-machine 'n) ; 4

;; Test: t8
(set-register-contents!
 count-leaves-n-machine 'tree t8)                 ; done
(start count-leaves-n-machine)                    ; done
(get-register-contents count-leaves-n-machine 'n) ; 8
