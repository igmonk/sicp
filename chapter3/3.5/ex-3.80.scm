;; Exercise 3.80
;;
;; A series RLC circuit consists of a resistor, a capacitor, and
;; an inductor connected in series, as shown in figure 3.36.
;;
;; If R, L, and C are the resistance, inductance, and capacitance,
;; then the relations between voltage (v) and current (i) for
;; the three components are described by the equations
;;
;; v(R) = i(R) * R
;;
;;            di(L)
;; v(L) = L * -----
;;             dt
;;
;;            du(C)
;; i(C) = C * -----
;;             dt
;;
;; and the circuit connections dictate the realations
;;
;; i(R) = i(L) = -i(C)
;; v(C) = v(L) + v(R)
;;
;; Combining these equations shows that the state of the circuit
;; (summarized by v(C), the voltage across the capacitor,
;; and i(L), the current in the inductor) is described by
;; the pair of differential equations
;;
;; dv(C)     i(L)
;; ----- = - ----
;;  dt        C
;;
;; di(L)   1          R
;; ----- = - * v(C) - - * i(L)
;;  dt     L          L
;;
;; The signal-flow diagram representing this system
;; of differential equations is shown in figure 3.37.
;;
;; Write a procedure RLC that takes as arguments the parameters
;; R, L, and C of the circuit and the time increment dt.
;;
;; In a manner similar to that of the RC procedure of exercise 3.73,
;; RLC should produce a procedure that takes the initial values
;; of the state variables, v(C0) and i(L0), and produces a pair (using cons)
;; of the streams of states v(C) and i(L).
;;
;; Using RLC, generate the pair of streams that models the behavior of
;; a series RLC circuit with R = 1 ohm, C = 0.2 farad, L = 1 henry,
;; dt = 0.1 second, and initial values iL0 = 0 amps and vC0 = 10 volts.

(load "workbook.scm")

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral-delayed (delay dvc) vc0 dt))
    (define il (integral-delayed (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams
                 (scale-stream vc (/ 1 L))
                 (scale-stream il (- (/ R L)))))
    (cons vc il)))

(define rlc-1 (RLC 1 1 0.2 0.1))
(define vc-il-streams-pair (rlc-1 10 0))

(load-option 'format)

(define (display-n n s-pair alias-1 alias-2)
  (if (<= n 0)
      'done
      (let ((s1 (car s-pair))
            (s2 (cdr s-pair)))
        (newline)        
        (display (format false "~a: ~a, ~a: ~a"
                         alias-1 (stream-car s1)
                         alias-2 (stream-car s2)))
        (display-n (- n 1)
                   (cons (stream-cdr s1)
                         (stream-cdr s2))
                   alias-1
                   alias-2))))

;; (display-n 10 vc-il-streams-pair "voltage" "current")
;;
;; voltage: 10, current: 0
;; voltage: 10, current: 1.
;; voltage: 9.5, current: 1.9
;; voltage: 8.55, current: 2.66
;; voltage: 7.220000000000001, current: 3.249
;; voltage: 5.5955, current: 3.6461
;; voltage: 3.77245, current: 3.84104
;; voltage: 1.8519299999999999, current: 3.834181
;; voltage: -.0651605000000004, current: 3.6359559
;; voltage: -1.8831384500000004, current: 3.2658442599999997
;;
;; ;Value: done
