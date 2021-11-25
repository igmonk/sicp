;; Binary Search Tree (BST)
;;
;; BST requires a comparator to compare two keys.
;;
;; BST Structure:
;;          Node: (entry left-branch right-branch)
;;         Entry: (key value)
;;           Key: symbol
;;
;; Node Structure:
;;            Key: anything that conforms to the provided comparator
;;          Value: whatever is inserted
;;
;; Operations:
;; - lookup
;; - insert!
;;
;; Resources:
;; - SRFI 128 - Comparators (reduced):
;;   https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/SRFI-128.html
;; - SRFI 128:
;;   https://srfi.schemers.org/srfi-128/srfi-128.html

(define (make-bst comp)

  ;; Node
  (define (make-node entry left right)
    (list entry left right))
  (define (make-leaf entry)
    (make-node entry '() '()))
  (define (entry node) (car node))
  (define (left node) (cadr node))
  (define (right node) (caddr node))
  (define (set-entry! node entry) (set-car! node entry))
  (define (set-left! node left) (set-car! (cdr node) left))
  (define (set-right! node right) (set-car! (cddr node) right))

  ;; Record
  (define (make-record key value) (list key value))
  (define (record-key record) (car record))
  (define (record-value record) (cadr record))
  (define (set-record-value! record value)
    (set-car! (cdr record) value))

  ;; Local state
  (let ((root '()))

    (define (lookup key)
      (define (inner node)
        (if (or (null? node))
            false
            (let ((entry-key (record-key (entry node))))
              (cond ((=? comp key entry-key)
                     (record-value (entry node)))
                    ((<? comp key entry-key)
                     (inner (left node)))
                    ((>? comp key entry-key)
                     (inner (right node)))))))
      (inner root))

    (define (insert! key value)
      (define (inner node)
        (let ((entry-key (record-key (entry node))))
          (cond ((=? comp key entry-key)
                 (set-record-value! (entry node) value))
                ((<? comp key entry-key)
                 (if (null? (left node))
                     (set-left! node (make-leaf (make-record key value)))
                     (inner (left node))))
                ((>? comp key entry-key)
                 (if (null? (right node))
                     (set-right! node (make-leaf (make-record key value)))
                     (inner (right node)))))))
      (if (null? root)
          (set! root (make-leaf (make-record key value)))
          (inner root)))

    ;; Alternative insert!
    ;;
    ;; (define (insert! key value)
    ;;   (define (inner parent-node set-branch! node)
    ;;     (if (null? node)
    ;;         (set-branch! parent-node
    ;;                      (make-leaf (make-record key value)))
    ;;         (let ((entry-key (record-key (entry node))))
    ;;           (cond ((=? comp key entry-key)
    ;;                  (set-record-value! (entry node) value))
    ;;                 ((<? comp key entry-key)
    ;;                  (inner node set-left-branch! (left-branch node)))
    ;;                 ((>? comp key entry-key)
    ;;                  (inner node set-right-branch! (right-branch node)))))))
    ;;   (if (null? root)
    ;;       (set! root (make-leaf (make-record key value)))
    ;;       (inner '() '() root)))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- BST" m))))

    dispatch))


;; Tests
;;
;; 1. Symbolic keys - symbol comparator
;;
;; (define symbol-comp (make-comparator symbol? symbol=? symbol<? symbol-hash))
;; (define bst1 (make-bst symbol-comp))
;;
;; ((bst1 'lookup) 'one) ; false

;; ((bst1 'insert!) 'one 1)
;; ((bst1 'insert!) 'two 2)
;; ((bst1 'insert!) 'three 3)
;; ((bst1 'insert!) 'four 4)
;; ((bst1 'insert!) 'five 5)

;; ((bst1 'lookup) 'one)   ; 1
;; ((bst1 'lookup) 'two)   ; 2
;; ((bst1 'lookup) 'three) ; 3
;; ((bst1 'lookup) 'four)  ; 4
;; ((bst1 'lookup) 'five)  ; 5
;; ((bst1 'lookup) 'six)   ; false
;; ((bst1 'lookup) 'seven) ; false


;; 2. Numeric keys - integer comparator
;;
;; (define int-comp (make-comparator integer? = < number-hash))
;; (define bst2 (make-bst int-comp))
;;
;; ((bst2 'lookup) 1) ; false

;; ((bst2 'insert!) 1 100)
;; ((bst2 'insert!) 2 200)
;; ((bst2 'insert!) 3 300)
;; ((bst2 'insert!) 4 400)
;; ((bst2 'insert!) 5 500)

;; ((bst2 'lookup) 1) ; 100
;; ((bst2 'lookup) 2) ; 200
;; ((bst2 'lookup) 3) ; 300
;; ((bst2 'lookup) 4) ; 400
;; ((bst2 'lookup) 5) ; 500
;; ((bst2 'lookup) 6) ; false
;; ((bst2 'lookup) 7) ; false
