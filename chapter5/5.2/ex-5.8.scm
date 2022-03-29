;; Exercise 5.8
;;
;; The following register-machine code is ambiguous,
;; because the label here is defined more than once:
;;
;; start
;;   (goto (label here))
;; here
;;   (assign a (const 3))
;;   (goto (label there))
;; here
;;   (assign a (const 4))
;;   (goto (label there))
;; there
;;
;; With the simulator as written, what will the contents of
;; register 'a' be when control reaches 'there'?
;;
;; Modify the extract-labels procedure so that the assembler
;; will signal an error if the same label name is used to
;; indicate two different locations.

(load "machine.scm")
(load "basic-machine-ext.scm")


;; The procedure extract-labels calls the top-level receive
;; (the one that was passed from the procedure assemble)
;; with the list of labels ordered as they appear in the
;; controller text.
;;
;; Upon looking up the label by its name, assoc returns
;; the first occurrence of that label in the list, and,
;; consequently, its corresponding instruction sequence.
;;
;; Hence, the instruction sequence that's labeled 'here'
;; earlier gets executed, whereas another labeled identically
;; never does.
;;
;; Expected result: 3

(define here-here-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

;; (start here-here-machine)                    ; done
;; (get-register-contents here-here-machine 'a) ; 3


;; After the suggested modification has been applied,
;; the same controller sequence signals an error upon
;; the machine creation:
;;
;; Label name already exists -- ASSEMBLE here


;; The following procedure has been introduced and installed
;; to check if the label with the given name already exists:

(define (label-exists? labels label-name)
  (assoc label-name labels))

;; See: assembler.scm
