;; Exercise 3.49
;;
;; Give a scenario where the deadlock-avoidance mechanism described above
;; does not work.
;;
;; (Hint: In the exchange problem, each process knows in advance which accounts
;; it will need to get access to. Consider a situation where a process
;; must get access to some shared resources before it can know
;; which additional shared resources it will require.)


;; See: https://web.mit.edu/6.005/www/fa15/classes/23-locks/
;;      https://web.mit.edu/6.005/www/fa15/classes/23-locks/#deadlock_rears_its_ugly_head
;;
;; Although lock ordering is useful (particularly in code like operating system kernels),
;; it has a number of drawbacks in practice.
;;
;; - First, it’s not modular — the code has to know about all the locks in the system,
;;   or at least in its subsystem.
;; - Second, it may be difficult or impossible for the code to know exactly
;;   which of those locks it will need before it even acquires the first one.
;;   It may need to do some computation to figure it out.
;;   Think about doing a depth-first search on the social network graph,
;;   for example — how would you know which nodes need to be locked, before
;;   you’ve even started looking for them?
