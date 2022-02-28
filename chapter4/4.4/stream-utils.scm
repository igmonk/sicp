;; Stream Utils

;; 'stream-append-delayed' and 'interleave-delayed' are
;; just like 'stream-append' and 'interleave' (section 3.5.3),
;; except that they take a delayed argument.
;; This postpones looping in some cases (ex. 4.71).

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

;; 'stream-flatmap' is the stream analog of the 'flatmap'
;; procedure introduced for ordinary lists in section 2.2.3.
;;
;; Unlike ordinary 'flatmap', the streams are accumulated
;; with an interleaving process, rather than simply
;; being appended (see ex. 4.72 and 4.73).

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

;; Generate a stream consisting of a single element:

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

;; Displaying streams

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


;; A simpler version of stream-flatmap (ex. 4.74).
;;
;; Can be used in negate, lisp-value, and find-assertions.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter stream-not-null? stream)))

(define (stream-not-null? s)
  (not (stream-null? s)))
