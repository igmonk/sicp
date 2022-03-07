;; Exercise 4.66
;;
;; Ben has been generalizing the query system to provide statistics
;; about the company. For example, to find the total salaries of
;; all the computer programmers one will be able to say

(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

;; In general, Ben's new system allows expressions of the form

(accumulation-function <variable>
                       <query pattern>)

;; where accumulation-function can be things like sum, average,
;; or maximum.
;;
;; Ben reasons that it should be a cinch to implement this.
;;
;; He will simply feed the query pattern to qeval.
;; This will produce a stream of frames. He will then pass
;; this stream through a mapping function that extracts the value of
;; the designated variable from each frame in the stream and
;; feed the resulting stream of values to the accumulation function.
;;
;; Just as Ben completes the implementation and is about to try it out,
;; Cy walks by, still puzzling over the 'wheel' query result
;; in exercise 4.65.
;;
;; When Cy shows Ben the system's response, Ben groans,
;; "Oh, no, my simple accumulation scheme won't work!"
;;
;; What has Ben just realized?
;; Outline a method he can use to salvage the situation.


;; Ben has just realized the accumulation function can be fed
;; an excessive amount of frames due to the way a rule is
;; applied.
;;
;; An example could be an endeavour to compute the total salaries
;; of all the 'wheels' in the organisation - the list that contains
;; duplicates (see ex. 4.55).
;;
;; A possible method Ben can use to salvage the situation could be
;; installing a filter that would filter out all the duplicates
;; from the stream of frames produced by the query pattern right
;; before feeding it to the accumulation function.
