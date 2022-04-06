;; 5.3 Storage Allocation and Garbage Collection
;;
;; Memory as Vectors
;;
;; In order to describe memory operations, two primitive
;; Scheme procedures for manipulating vectors are used:
;; - (vector-ref <vector> <n>)
;;   returns the n-th element of the vector
;; - (vector-set! <vector> <n> <value>)
;;   sets the n-th element of the vector to the designated value
;;
;; See the documentation:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html#Vectors


;; Vectors are heterogenous structures whose elements are indexed by
;; exact non-negative integers.
;;
;; The length of a vector is the number of elements that it contains.
;;
;; For example, a vector of length 3 containing
;; - the number zero in element 0
;; - the list (2 2 2 2) in element 1
;; - the string "Anna" in element 2
;; can be written as

'#(0 (2 2 2 2) "Anna") ; #(0 (2 2 2 2) "Anna")

(define v1 '#(0 (2 2 2 2) "Anna"))

(vector? v1) ; true
(vector-length v1) ; 3

(vector-ref v1 0) ; 0
(vector-ref v1 1) ; (2 2 2 2)
(vector-ref v1 2) ; "Anna"

(vector-ref v1 3)
;; Error: The object 3, passed as the second argument to vector-ref,
;;        is not in the correct range

(let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec)
;; #(0 ("Sue" "Sue") "Anna")
