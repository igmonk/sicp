;; Instruction Combiners

(load "instruction-seq.scm")
(load "list-utils.scm")


;; Append instruction sequences
;;
;; The procedure takes as arguments an arbitrary number of
;; instruction sequences that are to be executed sequentially
;; and returns an instruction sequence whose
;; - statements are the statements of all the sequences
;;   appended together
;; - modified registers are those registers that are modified by
;;   any of the sequences
;; - registers needed are those registers that must be initialized
;;   before the first sequence can be run (the registers needed by
;;   the first sequence), together with those registers needed by
;;   any of the other sequences that are not initialized (modified)
;;   by sequences preceding it.
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))
  ;; (if (null? seqs)
  ;;     (empty-instruction-sequence)
  ;;     (append-2-sequences
  ;;      (car seqs)
  ;;      (apply append-instruction-sequences (cdr seqs)))))


;; Preserving
;;
;; The procedure takes a list of registers regs and two instruction
;; sequences seq1 and seq2 that are to be executed sequentially.
;;
;; It returns an instruction sequence whose statements are
;; the statements of seq1 followed by the statements of seq2,
;; with appropriate 'save' and 'restore' instructions around seq1
;; to protect the registers in regs that are modified by seq1
;; but needed by seq2.
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))


;; Tack on
;;
;; The procedure is used by compile-lambda to append
;; a procedure body to another sequence, where the body
;; is not 'in line' to be executed as part of the combined
;; sequence and its register use has no impact on the register
;; use of the sequence in which it is embedded.
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))


;; Parallel combiner
;;
;; The combiner appends the two alternative branches that
;; follow a test (usd by compile-if and compile-procedure-call).
;;
;; The two branches will never be executed sequentially;
;; for any particular evaluation of the test, one branch
;; of the other will be entered.
;;
;; The registers needed by the second branch are still needed
;; by the combined sequence, even if these are modified by
;; the first branch.
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))
