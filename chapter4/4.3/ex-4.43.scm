;; Exercise 4.43
;;
;; Use the amb evaluator to solve the following puzzle:
;;
;; Mary Ann Moore's father has a yacht and so has each of his
;; four friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood,
;; and Dr. Parker.
;;
;; Each of the five also has one daughter and each has named
;; his yacht after a daughter of one of the others.
;;
;; Sir Barnacle's yacht is the Gabrielle, Mr. Moore owns the Lorna;
;; Mr. Hall the Rosalind. The Melissa, owned by Colonel Downing,
;; is named after Sir Barnacle's daughter.
;; Gabrielle's father owns the yacht that is named after
;; Dr. Parker's daughter.
;;
;; Who is Lorna's father?
;;
;; Try to write the program so that it runs efficiently
;; (see exercise 4.40).
;;
;; Also determine how many solutions there are if we are not told
;; that Mary Ann's last name is Moore.


;; By taking the fathers' frame of reference the task becomes trivial,
;; since the fathers are connected directly to both daughters and yachts.
;;
;; The following can be deducted and excluded from amb:
;; 
;; 1) Mary Ann Moore's father has a yacht
;;      => Mr. Moore's daughter is Mary Ann
;; 2) Sir Barnacle's yacht is the Gabrielle
;;      => Sir Barnacles's daughter is not Gabrielle
;; 3) Mr. Hall owns the Rosalind
;;      => Mr. Hall's daughter is not Rosalind
;; 4) The Melissa is owned by Colonel Downing
;;      => Colonel Downing's daughter is not Melissa
;;      => Dr. Parker owns the Mary Ann, since the yachts of others
;;         have been identified
;; 5) The Melissa is named after Sir Barnacle's daughter
;;      => Sir Barnacle's daughter is Melissa
;; 6) Gabrielle's father owns the yacht that is named after
;;    Dr. Parker's daughter
;;      => given evetything above, Gabrielle's father yacht is one of:
;;         - Colonel Downning's yacht
;;         - Mr. Hall's yacht
;;      => Dr. Parker's daughter is not Gabrielle

(define (fathers)
  (let ((hood-yacht 'gabrielle)
        (moore-yacht 'lorna)
        (hall-yacht 'rosalind)
        (downing-yacht 'melissa)
        (parker-yacht 'maryann))
    (let ((moore-daughter 'maryann)
          (hood-daughter 'melissa)
          (downing-daughter (amb 'gabrielle 'lorna 'rosalind))
          (hall-daughter (amb 'gabrielle 'lorna))
          (parker-daughter (amb 'lorna 'rosalind)))
      (let ((gabrielle-father-yacht
             (cond ((eq? 'gabrielle downing-daughter) downing-yacht)
                   ((eq? 'gabrielle hall-daughter) hall-yacht)
                   (else (amb)))))
        (require (eq? gabrielle-father-yacht parker-daughter)))
      (require
       (distinct? (list moore-daughter
                        downing-daughter
                        hall-daughter
                        hood-daughter
                        parker-daughter)))
      (list (list 'moore moore-daughter)
            (list 'downing downing-daughter)
            (list 'hall hall-daughter)
            (list 'hood hood-daughter)
            (list 'parker parker-daughter)))))

;; (fathers)
;; ((moore maryann) (downing lorna) (hall gabrielle) (hood melissa) (parker rosalind))
;;
;; try-again ; There are no more values
;;
;; Hence, Colonel Downing is Lorna's father.


;; If we are not told that Mary Ann's last name is Moore.
;;
;; The yacht ownership stays the same, while there are some
;; changes to the father-daughters relations.
;;
;; Mary Ann has to be added to the possible choices for
;; Coloner Downing, Mr. Hall and Dr. Parker. In addition,
;; Mr. Moore's daughter might be one of Gabrielle, Lorna,
;; Rosalind or Mary Ann, instead of being firmly defined.
;;
;; Moreover, the 6th deduction (mentioned above) needs to be
;; adjusted as follows:
;;
;; 6) Gabrielle's father owns the yacht that is named after
;;    Dr. Parker's daughter
;;      => given evetything above, Gabrielle's father yacht is one of:
;;         - Mr. Moore's yacht  <- new requirement
;;         - Colonel Downning's yacht
;;         - Mr. Hall's yacht
;;      => Dr. Parker's daughter is not Gabrielle


(define (fathers-2)
  (let ((hood-yacht 'gabrielle)
        (moore-yacht 'lorna)
        (hall-yacht 'rosalind)
        (downing-yacht 'melissa)
        (parker-yacht 'maryann))
    (let ((hood-daughter 'melissa)
          (moore-daughter (amb 'gabrielle 'lorna 'rosalind 'maryann))
          (downing-daughter (amb 'gabrielle 'lorna 'rosalind 'maryann))
          (hall-daughter (amb 'gabrielle 'lorna 'maryann))
          (parker-daughter (amb 'lorna 'rosalind 'maryann)))
      (let ((gabrielle-father-yacht
             (cond ((eq? 'gabrielle moore-daughter) moore-yacht)
                   ((eq? 'gabrielle downing-daughter) downing-yacht)
                   ((eq? 'gabrielle hall-daughter) hall-yacht)
                   (else (amb)))))
        (require (eq? gabrielle-father-yacht parker-daughter)))
      (require
       (distinct? (list moore-daughter
                        downing-daughter
                        hall-daughter
                        hood-daughter
                        parker-daughter)))
      (list (list 'moore moore-daughter)
            (list 'downing downing-daughter)
            (list 'hall hall-daughter)
            (list 'hood hood-daughter)
            (list 'parker parker-daughter)))))

;; (fathers-2)
;; try-again
;; try-again
;; try-again ; There are no more values

;; The result is:
;;
;; ((moore gabrielle) (downing rosalind) (hall maryann) (hood melissa) (parker lorna))
;; ((moore lorna) (downing maryann) (hall gabrielle) (hood melissa) (parker rosalind))
;; ((moore maryann) (downing lorna) (hall gabrielle) (hood melissa) (parker rosalind))
;;
;; Hence, the are 3 solutions if we are not told that Mary Ann's
;; last name is Moore.
