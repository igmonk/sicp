;; Exercise 5.4
;;
;; Specify register machines that implement each of
;; the following procedures.
;;
;; For each machine, write a controller instruction sequence
;; and draw a diagram showing the data paths.
;;
;; a. Recursive exponentiation:

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; b. Iterative exponentiation:

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))


;; a. Recursive exponentiation

(controller
 (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
 after-expt
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 expt-done)

;; * no need to save/restore n around each subroutine call, since
;;   there is no need in retrieving its values in reverse order.
;; * no need to save/restore b around each subroutine call, since
;;   it remains unchanged.


;; b. Iterative exponentiation

(controller
 (assign counter (reg n))
 (assign product (const 1))
 expt-loop
   (test (op =) (reg counter) (const 0))
   (branch (label expt-done))
   (assign counter (op -) (reg counter) (const 1))
   (assign product (op *) (reg b) (reg product))
   (goto (label expt-loop))
 expt-done)
