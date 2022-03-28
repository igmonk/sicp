;; Branch Instruction
;;
;; The execution procedure for a 'branch' instruction
;; checks the contents of the 'flag' register and either
;; - sets the contents of the 'pc' to the branch destination
;;   (if the branch is taken), or else
;; - advances the 'pc'
;;   (if the branch is not taken)

(load "register.scm")
(load "label.scm")
(load "pc.scm")

(define (install-branch assembler)
  (let ((extend-assemble (assembler 'extend-assemble))
        (lookup-label (assembler 'lookup-label)))

    (define (assemble-branch inst machine labels)
      (let ((dest (branch-dest inst))
            (flag (machine 'flag))
            (pc (machine 'pc)))
        (if (label-exp? dest)
            (let ((insts
                   (lookup-label labels (label-exp-label dest))))
              (lambda ()
                (if (get-contents flag)
                    (set-contents! pc insts)
                    (advance-pc pc))))
            (error "Bad BRANCH instruction -- ASSEMBLE-BRANCH" inst))))

    (define (branch-dest branch-instruction)
      (cadr branch-instruction))

    (extend-assemble 'branch assemble-branch)))
