;; Compiler Tests

(load "compiler-factory.scm")
(load "io-utils.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(define compiled-factorial
  (compile
   '(define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
   'val
   'next))

;; Save to a file if needed.
(instruction-seq->file compiled-factorial "factorial.obj")


;; Compile and go
(load "compile-and-go.scm")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;; (total-pushes = 0 maximum-depth = 0)

;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5) ;; (total-pushes = 31 maximum-depth = 14)

;;; EC-Eval value:
120

;; The interpreted version required 144 pushes and
;; a maximum stack depth of 28!
;;
;; This illustrates the optimization that results from
;; the compilation strategy.



;; Compile and run
(load "compile-and-run.scm")

;; Add the primitive to the list of primitive procedures
;; of the machine and reset the global environment to
;; reinitialize the machine's primitive procedures:

(append! primitive-procedures
         (list (list 'compile-and-run compile-and-run)))

(reset-env!)


;; Set flag to false and re-run the machine:

(set-register-contents! ec-eval-ext-machine 'flag false)
(start ec-eval-ext-machine)


;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(total-pushes = 6 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 31 maximum-depth = 14)
;;; EC-Eval value:
120

;;; EC-Eval input:
(compile-and-run
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(total-pushes = 6 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 10)

(total-pushes = 887 maximum-depth = 29)
;;; EC-Eval value:
55

;;; EC-Eval input:



;; RCEPL machine
(load "rcepl-machine.scm")

;; Set flag to false and start the machine:
(set-register-contents! rcepl-machine 'flag false)
(start rcepl-machine)


;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 26 maximum-depth = 14)
;;; EC-Eval value:
120

;;; EC-Eval input:
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 10)

(total-pushes = 882 maximum-depth = 29)
;;; EC-Eval value:
55

;;; EC-Eval input:
