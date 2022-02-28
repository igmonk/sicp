;; Data Base

(load "../../chapter3/3.3/table-obj-2d.scm")
(load "vars.scm")
(load "rule.scm")

(define (make-data-base)
  (let ((index-storage (make-table))
        (THE-ASSERTIONS the-empty-stream)
        (THE-RULES the-empty-stream))

    ;; In addition to storing all assertions in one big stream,
    ;; we store all assertions whose cars are constant symbols
    ;; in separate streams, in a table indexed by the symbol.
    ;;
    ;; Cleverer methods could also take advantage of information in
    ;; the frame, or try also to optimize the case where the car of
    ;; the pattern is not a constant symbol.
    (define (fetch-assertions pattern frame)
      (if (use-index? pattern)
          (get-indexed-assertions pattern)
          (get-all-assertions)))

    (define (get-all-assertions) THE-ASSERTIONS)

    (define (get-indexed-assertions pattern)
      (get-stream (index-key-of pattern) 'assertion-stream))

    ;; When fetching rules that might match a pattern whose car
    ;; is a constant symbol we fetch all rules whose conclusions
    ;; start with a variable as well as those whose conclusions
    ;; have the same car as the pattern.
    (define (fetch-rules pattern frame)
      (if (use-index? pattern)
          (get-indexed-rules pattern)
          (get-all-rules)))

    (define (get-all-rules) THE-RULES)

    (define (get-indexed-rules pattern)
      (stream-append
       (get-stream (index-key-of pattern) 'rule-stream)
       (get-stream '? 'rule-stream)))

    ;; Add assertions and rules to the data base.
    ;;
    ;; Each item is stored in the index, if appropriate, and
    ;; in a stream of all assertions or rules in the data base.
    (define (add-rule-or-assertion! assertion)
      (if (rule? assertion)
          (add-rule! assertion)
          (add-assertion! assertion)))

    (define (add-assertion! assertion)
      (store-assertion-in-index assertion)
      (let ((old-assertions THE-ASSERTIONS))
        (set! THE-ASSERTIONS
              (cons-stream assertion old-assertions))
        'ok))

    (define (add-rule! rule)
      (store-rule-in-index rule)
      (let ((old-rules THE-RULES))
        (set! THE-RULES (cons-stream rule old-rules))
        'ok))

    (define (store-assertion-in-index assertion)
      (if (indexable? assertion)
          (let ((key (index-key-of assertion)))
            (let ((current-assertion-stream
                   (get-stream key 'assertion-stream)))
              (put key
                   'assertion-stream
                   (cons-stream assertion
                                current-assertion-stream))))))

    (define (store-rule-in-index rule)
      (let ((pattern (conclusion rule)))
        (if (indexable? pattern)
            (let ((key (index-key-of pattern)))
              (let ((current-rule-stream
                     (get-stream key 'rule-stream)))
                (put key
                     'rule-stream
                     (cons-stream rule
                                  current-rule-stream)))))))

    ;; The data-base index
    (define (indexable? pat)
      (or (constant-symbol? (car pat))
          (var? (car pat))))

    (define (index-key-of pat)
      (let ((key (car pat)))
        (if (var? key) '? key)))

    (define (use-index? pat)
      (constant-symbol? (car pat)))


    ;; Index storage procedures
    (define get (index-storage 'lookup-proc))
    (define put (index-storage 'insert-proc!))

    
    ;; 'get-stream' looks up a stream in the table and returns
    ;; an empty stream if nothing is stored there.
    (define (get-stream key1 key2)
      (let ((s (get key1 key2)))
        (if s s the-empty-stream)))

    
    ;; Interface to the rest of the system
    (define (dispatch m)
      (cond ((eq? m 'fetch-assertions) fetch-assertions)
            ((eq? m 'fetch-rules) fetch-rules)
            ((eq? m 'add-rule-or-assertion!) add-rule-or-assertion!)
            (else
             (error "Unknown operation MAKE-DATA-BASE" m))))

    dispatch))
