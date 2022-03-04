;; Exercise 4.63
;;
;; The following data base (see Genesis 4) traces the genealogy of
;; the descendants of Ada back to Adam, by way of Cain:

(load "test-utils.scm")

;; Run the driver loop
(query-driver-loop)

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

;; Formulate rules such as
;; 
;; "If S is the son of F, and F is the son of G,
;;  then S is the grandson of G" and
;; "If W is the wife of M, and S is the son of W,
;;  then S is the son of M"
;; (which was supposedly more true in biblical times than today)
;; that will enable the query system to find
;; - the grandson of Cain;
;; - the sons of Lamech;
;; - the grandsons of Methushael.
;;
;; (See exercise 4.69 for some rules to deduce
;; more complicated relationships.)


;; "If S is the son of F, and F is the son of G,
;;  then S is the grandson of G"

(assert!
 (rule (grandson ?g ?s)
       (and (son ?f ?s)
            (son ?g ?f))))


;; "If W is the wife of M, and S is the son of W,
;;  then S is the son of M"

(assert!
 (rule (son ?m ?s)
       (and (wife ?m ?w)
            (son ?w ?s))))


;; Find the grandson of Cain
(grandson Cain ?x)

;; Query results:
;;
;; (grandson cain irad)


;; Find the sons of Lamech
(son Lamech ?x)

;; Query results:
;;
;; (son lamech jubal)
;; (son lamech jabal)


;; Find the grandsons of Methushael
(grandson Methushael ?x)

;; Query results:
;;
;; (grandson methushael jubal)
;; (grandson methushael jabal)
