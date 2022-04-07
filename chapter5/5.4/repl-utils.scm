;; REPL Utils

(load "procedure.scm")

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display str)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
