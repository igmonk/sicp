;; Exercise 3.46
;;
;; Suppose that we implement test-and-set! using an ordinary procedure
;; as shown in the text, without attempting to make the operation atomic.
;;
;; Draw a timing diagram like the one in figure 3.29 to demonstrate
;; how the mutex implementation can fail by allowing two processes
;; to acquire the mutex at the same time.


;; Given two processes P1 and P2 using the same serializer, and,
;; consequently, the same mutex, the implementation can fail without
;; attempting to make the test-and-set! operation atomic.
;;
;; Consider two concurrent deposit operations performed by P1 and P2.
;;
;; P1: s(r,w)
;; P2: s(r,w)
;;
;; * s(r,w) = serialized read/write operation.
;;
;; Next, unfold the serialization to see its internals:
;;
;; P1: ma,r,w,mr
;; P2: ma,r,w,mr
;;
;; * ma = (mutex 'acquire)
;; * mr = (mutex 'release)
;;
;; Next, unfold the mutex acquire operations to see their internals:
;;
;; P1: tas,r,w,mr
;; P2: tas,r,w,mr
;;
;; * tas = test-and-set!
;;
;; In case test-and-set! is atomic, the state consistency is preserved.
;; Once a process has tested the cell and fuond it to be false,
;; the cell contents will be set to true before any other process
;; can test the cell:
;;
;; P1: tas r w mr           OR:    P1: tas <wait> r w mr
;; P2: tas <wait> r w mr           P2: tas r w mr
;; 
;; In case test-and-set! is not atomic, it can be considered
;; as an access operation followed by a set operation, allowing
;; for several processes to acquire the same mutex:
;;
;; P1: test,set,r,w,mr
;; P2: test,set,r,w,mr
;;
;; which may lead to an inconsistent state, where only one of
;; the deposit operations is registered in the system:
;;
;; P1: test set      r w   mr
;; P2:      test set r   w mr
