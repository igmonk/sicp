;; 3.3 Modeling with Mutable Data

;; Mutable List Structure

;; set-car!
(define x1 (list (list 'a 'b) 'c 'd))
(define y1 (list 'e 'f))
(set-car! x1 y1)

;; x1 ; ((e f) c d)

;; set-cdr!
(define x1 (list (list 'a 'b) 'c 'd))
(set-cdr! x1 y1)

;; x1 ; ((a b) e f)

;; cons
(define x1 (list (list 'a 'b) 'c 'd))
(define z1 (cons y1 (cdr x1)))

;; z1 ; ((e f) c d)


;; Sharing and identity

(define x (list 'a 'b))
(define z1 (cons x x))

;; z1 ; ((a b) a b)

;; z1 -> |x|x|
;;        | |
;;        |_↓
;;        |
;;        ↓
;; x -> |x|x|--->|x|/|
;;       |        |
;;       a        b
;;
;; The sharing of x by the car and cdr of z1 is a consequence of
;; the straightforward way in which cons is implemented.

(define z2 (cons (list 'a 'b) (list 'a 'b)))

;; z2 ; ((a b) a b)

;; z2 -> |x|x|--->|x|x|--->|x|/|
;;        |        |        |
;;        |        a        b
;;        |        |        |
;;         `----->|x|x|--->|x|/|
;;
;; In this structure, the pairs in the two (a b) lists are distinct,
;; although the actual symbols are shared.
;;
;; The two pairs are distinct because each call to cons returns a new pair.

;; When thought of as a list, z1 and z2 both represent 'the same' list,
;; ((a b) a b).
;;
;; In general, sharing is completely undetectable if we operate on lists
;; using only cons, car, and cdr. However, if we allow mutators on list structure,
;; sharing becomes significant.
;;
;; As an example of the difference that sharing can make,
;; consider the following procedure, which modifies the car of the structure
;; to which it is applied:

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; (set-to-wow! z1) ; ((wow b) wow b)
;; (set-to-wow! z2) ; ((wow b) a b)

;; x ; (wow b)

;; One way to detect sharing in list structures is to use the predicate eq?,
;; which was introduced in section 2.3.1 as a way to test
;; whether two symbols are equal.
;;
;; More generally, (eq? x y) tests whether x and y are the same object
;; (that is, whether x and y are equal as pointers).


;; Mutation is just assignment
;;
;; We can implement mutable data objects as procedures
;; using assignment and local state.

(define _cons cons)
(define _car car)
(define _cdr cdr)
(define _set-car! set-car!)
(define _set-cdr! set-cdr!)

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; Set the default values back
(define cons _cons)
(define car _car)
(define cdr _cdr)
(define set-car! _set-car!)
(define set-cdr! _set-cdr!)


;; (define p1 (cons 1 2))

;; (car p1) ; 1
;; (cdr p1) ; 2

;; (set-car! p1 3)
;; (set-cdr! p1 4)

;; (car p1) ; 3
;; (cdr p1) ; 4


;; Representing queues
;;
;; See: queue.scm


;; Representing tables
;;
;; One-dimensional table
;; See: table-1d.scm
;;
;; Two-dimensional table
;; See: table-2d.scm
;;
;; Multi-dimensional table, in which:
;; - valus are stored under an arbitrary number of keys
;; - different values may be stored under different numbers of keys
;; 
;; See: table-md-list.scm
;;      table-md-bst.scm

;; Creating local tables
;;
;; The lookup and insert! operations defined in table-1d.scm and table-2d.scm
;; take the table as an argument.
;;
;; This enables us to use programs that access more than one table.
;;
;; Another way to deal with multiple tables is to have separate lookup and
;; insert! procedures for each table.
;;
;; This can be done by representing a table procedurally,
;; as an object that maintains an internal table as part of its local state.
;; When sent an appropriate message, this "table object" supplies
;; the procedure with which to operate on the internal table.
;;
;; See: table-obj-2d.scm
