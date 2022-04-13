;; I/O Utils

(load "instruction-seq.scm")

(define (instruction-seq->file seq path)
  (let ((f (open-output-file path false)))
    (for-each (lambda (instruction)
                (write-line instruction f)
                (flush-output f))
              (statements seq))
    'ok))
