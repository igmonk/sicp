;; Environment
;;
;; An environment is a sequence of frames, each of which
;; is a table of bindings that associate variables with
;; their corresponding values.
;;
;; Each frame of an environment is represented as
;; a list of bindings, where each binding is a name-value pair.

(load "../5.4/frame.scm")
(load "../5.4/frame-binding.scm")

(define (make-environment)
  (make-environment-with (make-empty-frame) '()))

(define (make-environment-with frame base-env)

  (define (empty-environment?)
    (and (empty-frame? frame)
         (eq? base-env '()))) ;; no need in recursive check

  (define (extend-environment vars vals)
    (make-environment-with
     (make-frame
      (make-frame-bindings vars vals))
     dispatch))

  (define (lookup-variable-value var)
    (let ((binding (lookup-binding dispatch var)))
      (if (not binding)
          (error "Unbound variable" var)
          (let ((value (frame-binding-value binding)))
            (if (eq? value '*unassigned*)
                (error "Unassigned variable" var)
                value)))))
  
  (define (set-variable-value! var val)
    (let ((binding (lookup-binding dispatch var)))
      (if (not binding)
          (error "Unbound variable -- SET!" var)
          (frame-binding-set-value! val binding))))

  (define (define-variable! var val)
    (let ((binding (find-binding var frame)))
      (if (not binding)
          (add-binding-to-frame!
           (make-frame-binding var val)
           frame)
          (frame-binding-set-value! val binding))))

  (define (undefine-variable! var)
    (remove-binding! var frame))

  (define (lookup-binding env var)
    (if (env 'empty?)
        false
        (let ((binding (find-binding var (env 'get-frame))))
          (if (not binding)
              (lookup-binding (env 'get-base-env) var)
              binding))))

  ;; Interface to the rest of the system
  (define (dispatch m)
    (cond ((eq? m 'empty?) (empty-environment?))
          ((eq? m 'extend) extend-environment)
          ((eq? m 'get-frame) frame)
          ((eq? m 'get-base-env) base-env)
          ((eq? m 'lookup-variable-value) lookup-variable-value)
          ((eq? m 'set-variable-value!) set-variable-value!)
          ((eq? m 'define-variable!) define-variable!)
          ((eq? m 'undefine-variable!) undefine-variable!)
          (else (error "Unknown operation -- MAKE-ENVIRONMENT" m))))

  dispatch)


;; Syntactic sugar to allow for ordinary procedural syntax use
;; to access the local procedures of objects.

(define (extend-environment vars vals env)
  ((env 'extend) vars vals))

(define (lookup-variable-value var env)
  ((env 'lookup-variable-value) var))

(define (set-variable-value! var val env)
  ((env 'set-variable-value!) var val))

(define (define-variable! var val env)
  ((env 'define-variable!) var val))

(define (undefine-variable! var env)
  ((env 'undefine-variable!) var))
