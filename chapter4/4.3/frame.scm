;; Environment Frame
;;
;; A frame is represented as a list of bindings,
;; where each binding is a name-value pair.

(load "frame-binding.scm")

(define (make-empty-frame) (make-frame '()))

(define (make-frame bindings)

  (define (empty-frame?) (null? bindings))

  (define (add-binding! binding)
    (set! bindings (cons binding bindings)))

  (define (find-binding var)
    (define (iter bs)
      (cond ((null? bs) false)
            ((eq? var (frame-binding-name (car bs)))
             (car bs))
            (else (iter (cdr bs)))))
    (iter bindings))

  (define (remove-binding! var)
    (define (inner bs)
      (cond ((null? bs) bs)
            ((eq? var (frame-binding-name (car bs)))
             (cdr bs))
            (else (cons (car bs)
                        (inner (cdr bs))))))
    (set! bindings (inner bindings)))
  
  ;; Interface to the rest of the system
  (define (dispatch m)
    (cond ((eq? m 'empty?) empty-frame?)
          ((eq? m 'add-binding!) add-binding!)
          ((eq? m 'find-binding) find-binding)
          ((eq? m 'remove-binding!) remove-binding!)
          (else (error "Unknown operation -- MAKE-FRAME" m))))

  dispatch)


;; Syntactic sugar to allow for ordinary procedural syntax use
;; to access the local procedures of objects.

(define (empty-frame? frame) ((frame 'empty?)))

(define (add-binding-to-frame! binding frame)
  ((frame 'add-binding!) binding))

(define (find-binding var frame)
  ((frame 'find-binding) var))

(define (remove-binding! var frame)
  ((frame 'remove-binding!) var))
