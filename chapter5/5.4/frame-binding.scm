;; Frame Binding
;;
;; A frame binding is represented as a name-value pair.

(define (make-frame-binding name value)

  (define (set-value! val) (set! value val))

  (define (dispatch m)
    (cond ((eq? m 'name) name)
          ((eq? m 'value) value)
          ((eq? m 'set-value!) set-value!)
          (else (error "Unknown operation -- MAKE-FRAME-BINDING" m))))

  dispatch)


(define (make-frame-bindings names values)
  (if (= (length names) (length values))
      (map make-frame-binding names values)
      (if (< (length names) (length values))
          (error "Too many arguments supplied" names values)
          (error "Too few arguments supplied" names values))))


;; Syntactic sugar to allow for ordinary procedural syntax use
;; to access the local procedures of objects.

(define (frame-binding-name fb) (fb 'name))
(define (frame-binding-value fb) (fb 'value))
(define (frame-binding-set-value! val fb)
  ((fb 'set-value!) val))
