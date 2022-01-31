;; Exercise 4.17
;;
;; Draw diagrams of the environment in effect when evaluating
;; the expression <e3> in the procedure in the text,
;; comparing how this will be structured when definitions
;; are interpreted sequentially with how it will be structured
;; if definitions are scanned out as described.


;; a. Sequential interpretation
;;
;; (lambda <vars>
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>)
;; 
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env       ?---↓
;;                |
;; ---------------|------------------------------------------------------------------
;;                |           ↑              ↑
;;                |           |              |
;;                |           |            E1|<vars>:<vals>
;;                |           |              |u:<e1>
;;                ↓           |              |v:<e2>
;;              |x|x|---------↑
;;               |
;;               ↓
;;     parameters: <vars>
;;     body: (define ...)


;; b. Scanned out definitions
;;
;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>))
;; 
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env       ?---↓
;;                |
;; ---------------|------------------------------------------------------------------
;;                |           ↑              ↑
;;                |           |              |
;;                |           |            E1|<vars>:<vals>
;;                |           |            __|_____________
;;                ↓           |              ↑
;;              |x|x|---------↑              |
;;               |                           |
;;               ↓                         E2|u:'*unassigned*  
;;     parameters: <vars>                    |v:'*unassigned*
;;     body: (define ...)


;; Why is there an extra frame in the transformed program?

;; An extra frame gets created due to the 'let' form, that
;; is essentially syntactic sugar for a lambda. Upon lambda
;; execution a new environment/frame is created in which
;; the body of a lambda is evaluated.


;; Explain why this difference in environment structure can never
;; make a difference in the behavior of a correct program.

;; If by a correct program is meant the one that keeps its internal
;; definitions on top, before any expression that uses them
;; gets evaluated, it does not benefit from yet another frame,
;; which does essentially the same that the internal definitions do.


;; Design a way to make the interpreter implement
;; the "simultaneous" scope rule for internal definitions
;; without constructing the extra frame.

;; The design of such a solution could do a simple rearrangement,
;; where all the internal definitions (or, at least, those that
;; are used in the internal expressions) would be moved to the top.
;;
;; All the internal expressions, including definitions, are
;; evaluated in order (see eval-sequence).
