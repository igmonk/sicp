;; Exercise 1.41
;;
;; Define a procedure 'double' that takes a procedure of one argument as argument
;; and returns a procedure that applies the original procedure twice.
;;
;; For example, if inc is a procedure that adds 1 to its argument,
;; then (double inc) should be a procedure that adds 2.

(load "workbook.scm")

(define (double f)
  (lambda (x)
    (f (f x))))

;; ((double inc) 1)  ; 3  <- (inc (inc 1))
;; ((double inc) 10) ; 12 <- (inc (inc 10))

;; (((double double) inc) 1) ; 5
;;
;; Evaluation:
;;
;; (((double double) inc) 1)
;; ((double (double inc)) 1)
;; ((double inc) ((double inc) 1))
;; ((double inc) (inc (inc 1)))
;; (inc (inc (inc (inc 1))))


;; What value is returned by
;;
;; (((double (double double)) inc) 5)
;;
;; (((double (double double)) inc) 5) ; 21

;; Evaluation:
;;
;; (((double (double double)) inc) 5)
;; (((double double) ((double double) inc)) 5)
;; (((double double) (double (double inc))) 5)
;; ((double (double (double (double inc)))) 5)
;; ((double (double (double inc))) ((double (double (double inc))) 5))
;; ((double (double inc)) ((double (double inc)) ((double (double (double inc))) 5)))
;; ((double inc) ((double inc) ((double (double inc)) ((double (double (double inc))) 5))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double (double (double inc))) 5)))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double (double inc)) ((double (double inc)) 5))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double (double inc)) 5)))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) 5))))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) (inc (inc 5)))))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) (inc (inc (inc (inc 5))))))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) ((double inc) (inc (inc (inc (inc (inc (inc 5)))))))))))
;; ((double inc) ((double inc) ((double inc) ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))
;; ((double inc) ((double inc) ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))
;; ((double inc) ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))
;; ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 6)))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 7))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 8)))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 9))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 10)))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 11))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc 12)))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc 13))))))))
;; (inc (inc (inc (inc (inc (inc (inc 14)))))))
;; (inc (inc (inc (inc (inc (inc 15))))))
;; (inc (inc (inc (inc (inc 16)))))
;; (inc (inc (inc (inc 17))))
;; (inc (inc (inc 18)))
;; (inc (inc 19))
;; (inc 20)
;; 21


;; Remarkable, it is important in what order we are doubling operations,
;; (or, more generally, what operation we are doubling)
;; since it directly influences the evaluation process.
;;
;; The same amount of 'double' with a slightly different layout results in
;; a completely different evaluation process and final result:
;;
;; ((double (double (double inc))) 5) ; 13

;; Evaluation:
;;
;; ((double (double (double inc))) 5)
;; ((double (double inc)) ((double (double inc)) 5))
;; ((double inc) ((double inc) ((double (double inc)) 5)))
;; ((double inc) ((double inc) ((double inc) ((double inc) 5))))
;; ((double inc) ((double inc) ((double inc) (inc (inc 5)))))
;; ((double inc) ((double inc) (inc (inc (inc (inc 5))))))
;; ((double inc) (inc (inc (inc (inc (inc (inc 5)))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))
;; (inc (inc (inc (inc (inc (inc (inc 6)))))))
;; (inc (inc (inc (inc (inc (inc 7))))))
;; (inc (inc (inc (inc (inc 8)))))
;; (inc (inc (inc (inc 9))))
;; (inc (inc (inc 10)))
;; (inc (inc 11))
;; (inc 12)
;; 13
