;; Exercise 3.40
;;
;; Give all possible values of x that can result from executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))


;; Start with figuring out the two cases that conform to
;; a serialized access to the shared variable:
;;
;; 1) 1000000: P1 accesses twice and sets, then P2 accesses thrice and sets
;; 2) 1000000: P2 accesses thrice and sets, then P1 accesses twice and sets
;;
;; Next come the cases covering the interleaving of the events of P1 and P2.
;; These events together with their individual orderings can be depicted as:
;;
;;   P1: r r w
;;   P2: r r r w
;;
;;   where r stands for 'read' and denotes accessing x
;;         w stands for 'write' and denotes setting x
;;
;; The interleaving of the events that affects the consistency of results
;; happens when one process sets the shared variable in between a pair of
;; events of another process.
;;
;; The number of these possibilities equal to the cumulative number of gaps
;; found in the event chains of the processes:
;;
;;   P1: r r w   => 2 gaps
;;   P2: r r r w => 3 gaps
;;
;; That can result in the following additional cases:
;;
;; 3)   1000: r r   w       | Both P1 and P2 access all, then P1 sets,
;;            r r r   w     | then P2 sets
;;
;; 4)    100: r r     w     | Both P1 and P2 access all, then P2 sets,
;;            r r r w       | then P1 sets
;;
;; 5)  10000: r r w         | P1 accesses twice, P2 accesses twice, P1 sets,
;;            r r   r w     | P2 accesses once and sets
;;
;; 6) 100000: r r w         | P1 accesses twice, P2 accesses once, P1 sets,
;;            r     r r w   | P2 accesses twice and sets
;;
;; 7)  10000: r       r w   | P1 accesses once, P2 accesses thrice and sets,
;;            r r r w       | P1 accesses once and sets
;;
;;
;; See a more detailed answer below (section a.)


;; Which of these possibilities remain if we instead use
;; serialized procedures:

(define x 10)
(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;; The ones that conform to serialized access to the shared variable:
;;
;; 1) 1000000: P1 accesses twice and sets, then P2 accesses thrice and sets
;; 2) 1000000: P2 accesses thrice and sets, then P1 accesses twice and sets


;; a. Extended Answer
;;
;; Given the P1 and P2 processes and their corresponding events:
;;   P1 ordered events (access, set): a, b, w1
;;   P2 ordered events (access, set): x, y, z, w2
;;
;; A direct approach to find all the possible results is to
;; 1. Identify all the possible orderings
;; 2. Group the found orderings
;;
;; 1. Identify all the possible orderings
;; 
;; Depending on the interleaving of the events of P1 and P2,
;; possible orderings for the events that are consistent with
;; the individual orderings for the two processes:
;;
;; a,b,w1,x,y,z,w2
;; a,b,x,w1,y,z,w2  a,x,b,w1,y,z,w2
;; a,b,x,y,w1,z,w2  a,x,b,y,w1,z,w2  a,x,y,b,w1,z,w2
;; a,b,x,y,z,w1,w2  a,x,b,y,z,w1,w2  a,x,y,b,z,w1,w2  a,x,y,z,b,w1,w2
;; a,b,x,y,z,w2,w1  a,x,b,y,z,w2,w1  a,x,y,b,z,w2,w1  a,x,y,z,b,w2,w1  a,x,y,z,w2,b,w1
;;
;;                  x,a,b,w1,y,z,w2  x,a,y,b,w1,z,w2  x,a,y,z,b,w1,w2  x,a,y,z,w2,b,w1
;;                  x,a,b,y,w1,z,w2  x,y,a,b,w1,z,w2  x,y,a,z,b,w1,w2  x,y,a,z,w2,b,w1
;;                  x,a,b,y,z,w1,w2                   x,y,z,a,b,w1,w2  x,y,z,a,w2,b,w1
;;                  x,a,b,y,z,w2,w1  x,a,y,b,z,w1,w2                   x,y,z,w2,a,b,w1
;;                                   x,y,a,b,z,w1,w2  x,a,y,z,b,w2,w1
;;                                                    x,y,a,z,b,w2,w1
;;                                   x,a,y,b,z,w2,w1  x,y,z,a,b,w2,w1
;;                                   x,y,a,b,z,w2,w1
;;
;; Total count: 45
;;
;;
;; 2. Grouping:
;;
;; 1) P1 accesses twice and sets, then P2 accesses thrice and sets
;;
;;    a,b,w1,x,y,z,w2
;;
;;    Count: 1, Result: (* 100 100 100) = 1000000
;;
;; 2) P2 accesses thrice and sets, then P1 accesses twice and sets
;;
;;    x,y,z,w2,a,b,w1
;;
;;    Count: 1, Result: (* 1000 1000) = 1000000
;;
;; 3) Both P1 and P2 access all, then P1 sets, then P2 sets
;;
;;    a,b,x,y,z,w1,w2
;;    a,x,b,y,z,w1,w2
;;    a,x,y,b,z,w1,w2
;;    a,x,y,z,b,w1,w2
;;    x,a,b,y,z,w1,w2
;;    x,a,y,b,z,w1,w2
;;    x,y,a,b,z,w1,w2
;;    x,a,y,z,b,w1,w2
;;    x,y,a,z,b,w1,w2
;;    x,y,z,a,b,w1,w2
;;
;;    Count: 10, Result: (* 10 10 10) = 1000
;;
;; 4) Both P1 and P2 access all, then P2 sets, then P1 sets
;;
;;    a,b,x,y,z,w2,w1
;;    a,x,b,y,z,w2,w1
;;    a,x,y,b,z,w2,w1
;;    a,x,y,z,b,w2,w1
;;    x,a,b,y,z,w2,w1
;;    x,a,y,b,z,w2,w1
;;    x,y,a,b,z,w2,w1
;;    x,a,y,z,b,w2,w1
;;    x,y,a,z,b,w2,w1
;;    x,y,z,a,b,w2,w1
;;
;;    Count: 10, Result: (* 10 10) = 100
;;
;; 5) P1 accesses twice, P2 accesses twice, P1 sets, P2 accesses once and sets
;;
;;    a,b,x,y,w1,z,w2
;;    a,x,b,y,w1,z,w2
;;    a,x,y,b,w1,z,w2
;;    x,a,b,y,w1,z,w2
;;    x,a,y,b,w1,z,w2
;;    x,y,a,b,w1,z,w2
;;
;;    Count: 6, Result: (* 10 10 100) = 10000
;;
;; 6) P1 accesses twice, P2 accesses once, P1 sets, P2 accesses twice and sets
;;
;;    a,x,b,w1,y,z,w2
;;    x,a,b,w1,y,z,w2
;;    a,b,x,w1,y,z,w2
;;
;;    Count: 3, Result: (* 10 100 100) = 100000
;;
;; 7) P1 accesses once, P2 accesses thrice and sets, P1 accesses once and sets
;;
;;    a,x,y,z,w2,b,w1
;;    x,a,y,z,w2,b,w1
;;    x,y,a,z,w2,b,w1
;;    x,y,z,a,w2,b,w1
;;
;;    Count: 4, Result: (* 10 1000) = 10000
;;
;;
;; Hence, there are 7 groups of possible orderings, producing the following results:
;;
;; 1) 1000000: P1 accesses twice and sets, then P2 accesses thrice and sets
;; 2) 1000000: P2 accesses thrice and sets, then P1 accesses twice and sets
;; 3)    1000: Both P1 and P2 access all, then P1 sets, then P2 sets
;; 4)     100: Both P1 and P2 access all, then P2 sets, then P1 sets
;; 5)   10000: P1 accesses twice, P2 accesses twice, P1 sets,
;;             P2 accesses once and sets
;; 6)  100000: P1 accesses twice, P2 accesses once, P1 sets,
;;             P2 accesses twice and sets
;; 7)   10000: P1 accesses once, P2 accesses thrice and sets,
;;             P1 accesses once and sets
;;
;; Notice, 5) and 7) could form a common group, since their results are the same.
