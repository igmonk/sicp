;; Exercise 2.76
;;
;; As a large system with generic operations evolves,
;; new types of data objects or new operations may be needed.
;;
;; For each of the three strategies:
;; - generic operations with explicit dispatch
;; - data-directed style
;; - message-passing-style
;;
;; describe the changes that must be made to a system
;; in order to add new types or new operations.
;;
;; Which organization would be most appropriate for a system
;; in which new types must often be added?
;;
;; Which would be most appropriate for a system
;; in which new operations must often be added?


;; 1. Generic operations with explicit dispatch
;;
;; a) To add new types:
;;    - implement a set of procedures and ensure the uniqueness of
;;      their names across all existing representations
;;    - add a predicate responsible for recognizing the type
;;      based on the type tag
;;    - extend each generic selector to include the new type
;;
;; b) To add new operations:
;;    - extend each representation with the new operation
;;      while ensuring the uniqueness of their names
;;    - implement another generic method to account for
;;      the newly added operation
;;
;;
;; 2. Data-directed dispatch
;;
;; a) To add new types:
;;    - implement a set of procedures as a new package
;;    - interface the necessary internal procedures of the package
;;      to the rest of the system
;;
;; b) To add new operations:
;;    - extend each representation with the new operation
;;    - interface the newly added operation to the rest of the system
;;    - define a generic selector for the newly added operation
;;
;;
;; 3. Message passing style
;;
;; a) To add new types:
;;    - add a data object answerable for dispatching on operation names
;;
;; b) To add new operations:
;;    - extend each data object to include the new operation


;; Which organization would be most appropriate for a system
;; in which new types must often be added?

;; With a well-defined set of operations, both message passing style and
;; data-directed dispatch would be an appropriate choice,
;; since they only involve adding a new data object and a procedure package,
;; respectively. No changes for a high-level code are required.
;;
;; If, however, interfacing the necessary internal procedures is painful
;; (data-directed approach), the message passing style could be more
;; convenient.
;;
;; Explicit dispatch is the least appropriate for such a system,
;; since it requires changes to both data representation and
;; high-level code, while imposing the need to keep
;; the operation names unique.


;; Which would be most appropriate for a system
;; in which new operations must often be added?
;;
;; Message passing style seems to be most appropriate for a system
;; in which new operations must often be added, since it only requires
;; extending each data object to include the new operation
;; without the need to define new generic selector.
;;
;; If, however, defining the new generic selector is not an issue,
;; both the message passing and data-directed approaches are equally
;; convenient.
;;
;; Explicit dispatch is the least appropriate for such a system,
;; since it requires changes to both data representation and
;; high-level code, while imposing the need to keep
;; the operation names unique.
