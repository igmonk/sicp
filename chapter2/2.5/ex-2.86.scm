;; Exercise 2.86
;;
;; Suppose we want to handle complex numbers whose real parts,
;; imaginary parts, magnitudes, and angles can be either ordinary numbers,
;; rational numbers, or other numbers we might wish to add to the system.
;;
;; Describe and implement the changes to the system needed to accommodate this.
;; You will have to define operations such as 'sine' and 'cosine' that
;; are generic over ordinary numbers and rational numbers.


;; The foollowing changes have to be made:
;;
;; 1. Integer and Real numbers packages
;;
;;    Define and install in the dispatch table the following operations:
;;    - sine
;;    - cosine
;;    - square
;;    - sqrt
;;    - atan2
;;
;;    Note: there is no need to define the counterparts of these operations
;;          for the Rational numbers package due to the coercion mechanism.
;;          Each time one of these operations is not found in the table for
;;          rational numbers, the argument will be raised to the Real type,
;;          where the required operation is defined.
;;
;; 2. Rectangular and Polar complex numbers package
;;
;;    Make use of the generic add, mul, sine, cosine, square, sqrt
;;    and atan2 procedures.
;;
;; 3. Complex numbers package
;;
;;    Make use of the generic add, sub, mul, div and equ? procedures.
;;
;; See: arith_num_package.scm
