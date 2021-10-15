;; A set of useful definition used for exporting procedures
;; as an alternative to a stateful data structure
;; (for example, an operation-type table for data-directed programming).

(define (export-def op type item) (list op type item))
(define (export-def-op def) (car def))
(define (export-def-type def) (cadr def))
(define (export-def-item def) (caddr def))

(define (get-export-def op type export-defs)
  (if (null? export-defs)
      (error "No method for these types -- GET-EXPORT-DEF"
             (list op type))
      (let ((def (car export-defs)))
        (if (and (equal? (export-def-op def) op)
                 (equal? (export-def-type def) type))
            (export-def-item def)
            (get-export-def op type (cdr export-defs))))))
