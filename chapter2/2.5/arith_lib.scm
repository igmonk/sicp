;; Arithmetic system library
;;
;; Comprises the procedures intended to aid when building an arithmetic
;; numbers package and extending it with new types.
;;
;; The following components are part of the lib:
;; - type tag system
;; - apply-generic

(load "../../common.scm")
(load "../export-defs.scm")


;; Type tag system

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (type-tag datum)
  (cond ((exact-integer? datum) 'integer)
        ((real? datum) 'real)
        ((pair? datum) (car datum))
        ((boolean? datum) 'bool) ;; Alternatively, adjust the procedure drop.
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))


;; Apply Generic
;;
;; (with the wishful thinking strategy in mind)
;;
;; If there is a procedure defined for the given arguments,
;; it is applied, followed by the application of drop.
;;
;; Otherwise, we perform the equality check of the argument types.
;;
;; If all the types are equal, before giving up we can check if
;; they all can be raised one level up the number hierarchy and
;; call apply-generic recursively if they can.
;;
;; If not all the types are equal, we raise all the arguments
;; to the hierarchy level of the argument at the highest one
;; and make another recursive call.
;;
;; Notice, when the operation is of type 'raise' or 'project',
;; the procedure drop is not applied, otherwise the program
;; ends up in the endless recursion.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            (if (safe-to-drop? op) (drop result) result))
          (if (all-equal? type-tags)
              (if (first-raisable? type-tags)
                  (apply apply-generic (cons op (map raise args)))
                  (error "No method for these types"
                         (list op type-tags)))
              (apply apply-generic (cons op (raise-to-the-highest args))))))))


;; Utils
;;
;; Utility procedures that as well may be inner procedures of apply-generic.

(define (safe-to-drop? proc)
  (and (not (equal? proc 'raise))
       (not (equal? proc 'project))))

(define (drop x)
  (if (get-project-fn (type-tag x))
      (let ((projected-x (project x)))
        (if (equ? x (raise projected-x))
            (drop projected-x)
            x))
      x))

(define (first-raisable? types)
  (and (not (null? types))
       (get-raise-fn (car types))))

;; Raise every number to the hierarchy level which
;; the number with the highest one resides at.
(define (raise-to-the-highest numbers)
  (map raise-n numbers (get-relative-depths numbers)))

(define (get-relative-depths numbers)
  (let ((abs-depths (get-absolute-depths numbers)))
    (let ((min-abs-depth (apply min abs-depths)))
      (map (lambda (abs-depth)
             (- abs-depth min-abs-depth))
           abs-depths))))

(define (get-absolute-depths numbers)
  (map get-absolute-depth numbers))

(define (get-absolute-depth number)
  (define (inner n result)
    (if (get-raise-fn (type-tag n))
        (inner (raise n) (inc result))
        result))
  (inner number 0))

;; Raise the argument n times up the hierarchy of types.
;; Implemented as repeated application of the procedure raise.
(define (raise-n x n)
  ((repeated raise n) x))

(define (get-raise-fn type)
  (get 'raise (list type)))

(define (get-project-fn type)
  (get 'project (list type)))
