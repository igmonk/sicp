;; Exercise 5.41
;;
;; Write a procedure find-variable that takes as arguments a variable
;; and a compile-time environment and returns the lexical address of
;; the variable with respect to that environment.
;;
;; For example, in the program fragment that is shown above,
;; the compile-time environment during the compilation of expression
;; <e1> is ((y z) (a b c d e) (x y)).
;;
;; Find-variable should produce
;;
;; (find-variable 'c '((y z) (a b c d e) (x y)))
;; (1 2)
;;
;; (find-variable 'x '((y z) (a b c d e) (x y)))
;; (2 0)
;;
;; (find-variable 'w '((y z) (a b c d e) (x y)))
;; not-found


;; Recall the code structure:

((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) <e1>)
      <e2>
      (+ c d x))))
 3
 4)


;; Implement the following data representations:
;;
;; - compile-time environment
;;   (see cenvironment.scm)
;; - compile-time environment frame
;;   (see cframe.scm)


;; Load the necessary definitions and run some tests:

(load "cenvironment.scm")

(define cenv1
  (make-cenvironment-with
   (make-cframe '(y z))
   (make-cenvironment-with
    (make-cframe '(a b c d e))
    (make-cenvironment-with
     (make-cframe '(x y))
     (make-cenvironment)))))

(find-variable 'c cenv1) ; (1 . 2)
(find-variable 'x cenv1) ; (2 . 0)
(find-variable 'w cenv1) ; not-found
