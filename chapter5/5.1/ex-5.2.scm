;; Exercise 5.2
;;
;; Use the register-machine language to describe
;; the iterative factorial machine of exercise 5.1.

(controller
 (assign product (const 1))
 (assign counter (const 1))
 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label fact-done))
   (assign product (op *) (reg product) (reg counter))
   (assign counter (op +) (reg counter) (const 1))
   (goto (label test-counter))
 fact-done)
