;; Exercise 3.50
;;
;; Complete the following definition, which generalizes 'stream-map'
;; to allow procedures that take multiple arguments,
;; analogous to 'map' in section 2.2.3, footnote 12.
;;
;; (define (stream-map proc . argstreams)
;;   (if (<??> (car argstreams))
;;       the-empty-stream
;;       (<??>
;;        (apply proc (map <??> argstreams))
;;        (apply stream-map
;;               (cons proc (map <??> argstreams))))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; The procedure fails when argstreams is null,
;; as shown in the tests section below.


;; A slightly more sophisticated check that enables us to define
;; whether the given streams can be mapped, without failing on
;; (car argstreams), involves some ingenuity:
;;
;; mapable? ensures the given list of streams:
;; - is not null
;; - does not contain empty streams
;;   (by comparing the length of the original list
;;    with that of its filtered version)

(load "../../common.scm")

(define (mapable? streams)
  (and (not (null? streams))
       (= (length streams)
          (length (remove identity
                          (map stream-null? streams))))))

;; Tests
;;
;; (mapable? (list (cons-stream 1 2)
;;                 (cons-stream 3 4)
;;                 (cons-stream 5 6))) ; true
;;
;; (mapable? (list (cons-stream 1 2)
;;                 (cons-stream 3 4)
;;                 (cons-stream 5 6)
;;                 the-empty-stream)) ; false

(define (stream-map proc . streams)
  (if (not (mapable? streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map
                          (cons proc (map stream-cdr streams))))))

;; Tests
;;
;; (stream-map +) ; () / Error is thrown by the first stream-map
;;
;; (stream-map + the-empty-stream) ; ()
;;
;; (stream-map +
;;             (cons-stream 1 2)
;;             (cons-stream 3 4)
;;             (cons-stream 5 6))
;;
;; {9 ...}
;;
;; (stream-map *
;;             (cons-stream 1 2)
;;             (cons-stream 3 4)
;;             (cons-stream 5 6))
;;
;; {15 ...}
