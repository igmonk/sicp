;; Exercise 2.74
;;
;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company
;; consisting of a large number of independent divisions located all over the world.
;;
;; The company's computer facilities have just been interconnected by means of
;; a clever network-interfacing scheme that makes the entire network
;; appear to any user to be a single computer.
;;
;; Insatiable's president, in her first attempt to exploit the ability of
;; the network to extract administrative information from division files,
;; is dismayed to discover that, although all the division files
;; have been implemented as data structures in Scheme,
;; the particular data structure used varies from division to division.
;;
;; A meeting of division managers is hastily called to search for a strategy
;; to integrate the files that will satisfy headquarters' needs
;; while preserving the existing autonomy of the divisions.
;;
;; Show how such a strategy can be implemented with data-directed programming.
;;
;; As an example, suppose that each division's personnel records consist of
;; a single file, which contains a set of records keyed on employees' names.
;; The structure of the set varies from division to division.
;; Furthermore, each employee's record is itself a set
;; (structured differently from division to division) that contains information
;; keyed under identifiers such as address and salary. In particular:
;;
;; a. Implement for headquarters a 'get-record' procedure that retrieves
;;    a specified employee's record from a specified personnel file.
;;    The procedure should be applicable to any division's file.
;;    Explain how the individual divisions' files should be structured.
;;    In particular, what type information must be supplied?
;;
;; b. Implement for headquarters a 'get-salary' procedure that returns
;;    the salary information from a given employee's record
;;    from any division's personnel file.
;;    How should the record be structured in order to make this operation work?
;;
;; c. Implement for headquarters a 'find-employee-record' procedure.
;;    This should search all the divisions' files for the record of
;;    a given employee and return the record.
;;    Assume that this procedure takes as arguments an employee's name
;;    and a list of all the divisions' files.
;;
;; d. When Insatiable takes over a new company, what changes must be made
;;    in order to incorporate the new personnel information into the central system?


;; To achieve a modularized system design, which would address the issues of
;; a decentralized conglomerate company, it is reasonable to make use of
;; the data-directed programming technique.
;;
;; Each division of the company is expected to perform a set of operations
;; capable of working with the division's internal data representation
;; and equally interfaced to the rest of the system for external use.
;;
;; In that regard, the data-directed approach can be based on
;; a two-dimensional table that contains the possible operations on one axis
;; and the possible divisions (types) on the other axis.
;; 
;; |----------------------|-------------------------------------------------|
;; |                      |                      Types                      |
;; |      Operations      |-------------------------------------------------|
;; |                      |       Division-a       |       Division-b       |
;; |----------------------|------------------------|------------------------|
;; | get-personnel-file   | get-personnel-file-a   | get-personnel-file-b   |
;; | get-record           | get-record-a           | get-record-b           |
;; | get-salary           | get-salary-a           | get-salary-b           |
;; |----------------------|------------------------|------------------------|
;;
;; The interface is implemented as a single procedure that looks up
;; the combination of the operation name and argument type (division)
;; in the table to find the correct procedure to apply, and then
;; applies it to the contents of the argument.

;; Load the 'apply-generic', tagging and export procedure definitions
(load "workbook.scm")
(load "export-defs.scm")


;; Using apply-generic, define the generic operations as follows:

(define (get-record employee-name personnel-file)
  (apply-generic 'get-record employee-name personnel-file))

(define (get-salary employee-record)
  (apply-generic 'get-salary employee-record))

(define (find-employee-record employee-name personnel-files)
  (define (predicate record)
    (and (pair? record) (not (null? (cdr record)))))
  (filter predicate
          (map (lambda (personnel-file)
                 (get-record employee-name personnel-file))
               personnel-files)))

;; 'find-employee-record' finds the list of employees with a given name
;; across all divisions. Since the same employee name can be found
;; in more than one division, the results are combined in a list.


;; Next comes the decision on the data representation for employee records,
;; one for each division. The following formats are suggested:
;;
;; Division A (flat):
;;   (first-name last-name division address salary)
;;
;; Division B (nested):
;;   ((first-name last-name)
;;    (division address salary))


;; Next, each division is going to be represented as a set of
;; internal procedures interfaced to the rest of the system
;; by means of placing them into the operation-type table
;; (since the appropriate data structures - hash tables - have not been
;; discussed yet, it was decided to apply procedure export).


;; Division A package

(define (install-division-a-package)
  ;; internal procedures
  (define (make-employee-entry key record) (list key record))
  (define (employee-entry-key entry) (car entry))
  (define (employee-entry-record entry) (cadr entry))

  ;; A simple flat structure
  (define (make-employee-record
           first-name last-name division address salary)
    (list first-name last-name division address salary))
  (define (employee-record-first-name record) (car record))
  (define (employee-record-last-name record) (cadr record))
  (define (employee-record-division record) (caddr record))
  (define (employee-record-address record) (cadddr record))
  (define (employee-record-salary record) (caddddr record))

  (define (get-personnel-file)
    (list
     (make-employee-entry
      "Name11 LastName11"
      (make-employee-record
       "Name11" "LastName11" "Division A" "Address11" "110,110.00 USD"))
     (make-employee-entry
      "Name12 LastName12"
      (make-employee-record
       "Name12" "LastName12" "Division A" "Address12" "120,120.00 USD"))
     (make-employee-entry
      "Name13 LastName13"
      (make-employee-record
       "Name13" "LastName13" "Division A" "Address13" "130,130.00 USD"))
     (make-employee-entry
      "Name14 LastName14"
      (make-employee-record
       "Name14" "LastName14" "Division A" "Address14" "140,140.00 USD"))
     (make-employee-entry
      "Name15 LastName15"
      (make-employee-record
       "Name15" "LastName15" "Division A" "Address15" "150,150.00 USD"))
     (make-employee-entry
      "Name99 LastName99"
      (make-employee-record
       "Name99" "LastName99" "Division A" "Address99-A" "199,199.00 USD"))))

  (define (tag x) (attach-tag 'division-a x))

  (define (get-record employee-name personnel-file)
    (define (predicate entry)
      (equal? employee-name (employee-entry-key entry)))
    (apply-if-not-null employee-entry-record
                       (set-lookup predicate personnel-file)))

  (define (get-salary record)
    (employee-record-salary record))

  ;; interface to the rest of the system
  (list (export-def 'get-personnel-file
                    'division-a
                    (lambda _ (tag (get-personnel-file))))
        (export-def 'get-record
                    '(string division-a)
                    (lambda (x y) (tag (get-record x y))))
        (export-def 'get-salary '(division-a) get-salary)))

(define division-a-export-defs (install-division-a-package))


;; Division B package

(define (install-division-b-package)
  ;; internal procedures
  (define (make-employee-entry key record) (list key record))
  (define (employee-entry-key entry) (car entry))
  (define (employee-entry-record entry) (cadr entry))

  ;; A simple nested structure (as opposite to division-a's flat one)
  (define (make-employee-record
           first-name last-name division address salary)
    (list (list first-name last-name)
          (list division address salary)))
  (define (employee-record-first-name record) (caar record))
  (define (employee-record-last-name record) (cadar record))
  (define (employee-record-division record) (caadr record))
  (define (employee-record-address record) (cadadr record))
  (define (employee-record-salary record) (caddadr record))

  (define (get-personnel-file)
    (list
     (make-employee-entry
      "Name21 LastName21"
      (make-employee-record
       "Name21" "LastName21" "Division B" "Address21" "210,210.00 USD"))
     (make-employee-entry
      "Name22 LastName22"
      (make-employee-record
       "Name22" "LastName22" "Division B" "Address22" "220,220.00 USD"))
     (make-employee-entry
      "Name23 LastName23"
      (make-employee-record
       "Name23" "LastName23" "Division B" "Address23" "230,230.00 USD"))
     (make-employee-entry
      "Name24 LastName24"
      (make-employee-record
       "Name24" "LastName24" "Division B" "Address24" "240,240.00 USD"))
     (make-employee-entry
      "Name25 LastName25"
      (make-employee-record
       "Name25" "LastName25" "Division B" "Address25" "250,250.00 USD"))
     (make-employee-entry
      "Name99 LastName99"
      (make-employee-record
       "Name99" "LastName99" "Division B" "Address99-B" "299,299.00 USD"))))

  (define (tag x) (attach-tag 'division-b x))

  (define (get-record employee-name personnel-file)
    (define (predicate entry)
      (equal? employee-name (employee-entry-key entry)))
    (apply-if-not-null employee-entry-record
                       (set-lookup predicate personnel-file)))
  
  (define (get-salary record)
    (employee-record-salary record))

  ;; interface to the rest of the system
  (list (export-def 'get-personnel-file
                    'division-b
                    (lambda _ (tag (get-personnel-file))))
        (export-def 'get-record
                    '(string division-b)
                    (lambda (x y) (tag (get-record x y))))
        (export-def 'get-salary '(division-b) get-salary)))

(define division-b-export-defs (install-division-b-package))


;; When Insatiable takes over a new company, the following changes must be made
;; in order to incorporate the new personnel information into the central system:
;;
;; install the collection of procedures of the package corresponding to
;; the new personnel in the dispatch table (or make use of exports).


;; Tests
;;
;; (define personnel-file-a ((get 'get-personnel-file 'division-a)))
;; (define personnel-file-b ((get 'get-personnel-file 'division-b)))
;;
;;
;; a. Get Employee
;;
;; (define employee-11
;;   (get-record (attach-tag 'string "Name11 LastName11")
;;               personnel-file-a))
;;
;; (define employee-21
;;   (get-record (attach-tag 'string "Name21 LastName21")
;;               personnel-file-b))
;;
;; employee-11 ; (division-a "Name11" "LastName11" "Division A" "Address11" "110,110.00 USD")
;; employee-21 ; (division-b ("Name21" "LastName21") ("Division B" "Address21" "210,210.00 USD")
;;
;;
;; b. Get Salary
;;
;; (get-salary employee-11) ; "110,110.00 USD"
;; (get-salary employee-21) ; 210,210.00 USD
;;
;;
;; c. Find Employee Record
;;
;; (find-employee-record (attach-tag 'string "Name15 LastName15")
;;                       (list personnel-file-a
;;                             personnel-file-b))
;;
;; Value: ((division-a "Name15" "LastName15" "Division A" "Address15" "150,150.00 USD"))
;;
;;
;; (find-employee-record (attach-tag 'string "Name25 LastName25")
;;                       (list personnel-file-a
;;                             personnel-file-b))
;;
;; Value: ((division-b ("Name25" "LastName25") ("Division B" "Address25" "250,250.00 USD")))
;;
;;
;; (find-employee-record (attach-tag 'string "Name99 LastName99")
;;                       (list personnel-file-a
;;                             personnel-file-b))
;;
;; Value: ((division-a "Name99" "LastName99" "Division A" "Address99-A" "199,199.00 USD")
;;         (division-b ("Name99" "LastName99") ("Division B" "Address99-B" "299,299.00 USD")))
;;
;; (find-employee-record (attach-tag 'string "Not Found")
;;                       (list personnel-file-a
;;                             personnel-file-b))
;;
;; Value: ()


;; Utils

(define (caddddr x)
  (cadr (cdddr x)))

(define (caddadr x)
  (car (cddadr x)))

(define (set-lookup predicate set)
  (cond ((null? set) '())
        ((predicate (car set)) (car set))
        (else (set-lookup predicate (cdr set)))))

(define (apply-if-not-null proc x)
  (if (null? x) x (proc x)))

(define (get op type)
  (get-export-def op
                  type
                  (append division-a-export-defs
                          division-b-export-defs)))
