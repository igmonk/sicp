;; Exercise 3.30
;;
;; Figure 3.27 shows a 'ripple-carry adder' formed by stringing together
;; n full-adders.
;;
;; This is the simplest form of parallel adder for adding two
;; n-bit binary numbers.
;;
;; The inputs A1, A2, A3, ..., An and B1, B2, B3, ..., Bn are
;; the two binary numbers to be added (each Ak and Bk is a 0 or a 1).
;;
;; The circuit generates S1, S2, S3, ..., Sn, the n bits of the sum,
;; and C, the carry from the addition.
;;
;; Write a procedure 'ripple-carry-adder' that generates this circuit.
;;
;; The procedure should take as arguments three lists of n wires each -
;; the Ak, the Bk, and the Sk - and also another wire C.
;;
;; The major drawback of the ripple-carry adder is the need to wait for
;; the carry signals to propagate.
;;
;; What is the delay needed to obtain the complete output from
;; an n-bit ripple-carry adder, expressed in terms of the delays for
;; and-gates, or-gates, and inverters?

(load "workbook-sdc.scm")

(load-option 'format)

(define (ripple-carry-adder a-list b-list s-list c)
  (define (inner al bl sl)
    (if (null? al)
        (make-wire) ;; generate c(n) = 0
        (let ((c-out (make-wire)))
          (full-adder (car al)
                      (car bl)
                      (inner (cdr al) (cdr bl) (cdr sl))
                      (car sl)
                      c-out)
          c-out)))
  (full-adder (car a-list)
              (car b-list)
              (inner (cdr a-list) (cdr b-list) (cdr s-list))
              (car s-list)
              c)
  'ok)


;; Tests
;;
;; (define the-agenda (make-agenda))

;; Define ripple-carry adder I/O

;; (define rca-n 4)

;; (define a-list (gen-wire-list rca-n))
;; (define b-list (gen-wire-list rca-n))
;; (define s-list (gen-wire-list rca-n))

;; (define c (make-wire))

;; (probe 'carry c) ; carry 0 New-value = 0

;; (install-s-probes)
;; s1 0 New-value = 0
;; s2 0 New-value = 0
;; s3 0 New-value = 0
;; s4 0 New-value = 0

;; (ripple-carry-adder a-list b-list s-list c)

;; Test: add 0000 and 0000
;;
;; Expected: sum = 0000, carry = 0
;; 
;; (propagate) ; done
;; 
;; carry 2 New-value = 1
;; s4 5 New-value = 1
;; s3 5 New-value = 1
;; s2 5 New-value = 1
;; s1 5 New-value = 1
;; carry 7 New-value = 0
;; s4 10 New-value = 0
;; s3 10 New-value = 0
;; s2 10 New-value = 0
;; s1 10 New-value = 0
;; s4 15 New-value = 1
;; s3 15 New-value = 1
;; s2 15 New-value = 1
;; s1 15 New-value = 1
;; s4 20 New-value = 0
;; s3 20 New-value = 0
;; s2 20 New-value = 0
;; s1 20 New-value = 0
;; s3 22 New-value = 1
;; s2 22 New-value = 1
;; s1 22 New-value = 1
;; s3 27 New-value = 0
;; s2 27 New-value = 0
;; s1 27 New-value = 0
;;
;; Results:
;;
;; carry: time = 7, value = 0
;;
;; s1: time = 27, value = 0
;; s2: time = 27, value = 0
;; s3: time = 27, value = 0
;; s4: time = 20, value = 0
;;
;; Actual: sum = 0000 carry = 0


;; Test: add 0101 and 0110
;;
;; Expected: sum = 1011, carry = 0
;;
;; (set-signals! a-list (list 0 1 0 1))
;; (set-signals! b-list (list 0 1 1 0))
;;
;; (propagate) ; done
;;
;; s2 37 New-value = 1
;; s4 37 New-value = 1
;; s2 45 New-value = 0
;; s3 47 New-value = 1
;; s1 67 New-value = 1
;;
;; Results:
;;
;; carry: (unchanged)
;; 
;; s1: time = 67, value = 1
;; s2: time = 45, value = 0
;; s3: time = 47, value = 1
;; s4: time = 37, value = 1
;;
;; Actual: sum = 1011, carry = 0 (unchanged)


;; Test: add 1111 and 1111
;;
;; Expected: sum = 1110, carry = 1
;;
;; (set-signals! a-list (list 1 1 1 1))
;; (set-signals! b-list (list 1 1 1 1))
;;
;; (propagate) ; done
;;
;; s1 75 New-value = 0
;; s3 75 New-value = 0
;; carry 77 New-value = 1
;; s1 83 New-value = 1
;; s4 85 New-value = 0
;; s2 93 New-value = 1
;; s3 103 New-value = 1
;;
;; Results
;;
;; carry: time = 77, value = 1
;;
;; s1: time = 83, value 1
;; s2: time = 93, value 1
;; s3: time = 103, value 1
;; s4: time = 85, value 0
;;
;; Actual: sum = 1110, carry = 1


;; Utils

(define (gen-wire-list n)
  (if (> n 0)
      (cons (make-wire) (gen-wire-list (- n 1)))
      '()))

(define (install-s-probes)
  (define (probe-key i)
    (string->symbol (format false "s~a" i)))
  (define (inner sl i)
    (when (<= i rca-n)
      (probe (probe-key i) (car sl))
      (inner (cdr sl) (+ i 1))))
  (inner s-list 1))

(define (set-signals! wires values)
  (for-each (lambda (w v)
              (set-signal! w v))
            wires
            values))
