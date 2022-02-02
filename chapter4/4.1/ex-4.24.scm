;; Exercise 4.24
;;
;; Design and carry out some experiments to compare the speed of
;; the original metacircular evaluator with the version in this section.
;;
;; Use your results to estimate the fraction of time that is spent
;; in analysis versus execution for various procedures.


;; The results of time measurements are represented in the table below:
;;
;;  syntactic analysis |    expression    | run time | gc time | real time
;; --------------------|------------------|----------|---------|-----------
;;       preceding     | (factorial 1000) |       .1 |      0. |      .105
;;         mixed       | (factorial 1000) |      .19 |     .01 |      .203
;;       preceding     | (factorial 2000) |      .18 |      0. |      .198
;;         mixed       | (factorial 2000) |      .35 |      0. |      .355
;;       preceding     | (factorial 5000) |      .45 |     .01 |      .474
;;         mixed       | (factorial 5000) |      .89 |     .01 |      .911
;;
;; From the data above the following conclusions can be made:
;; 1) on average, it takes twice as much time to evaluate an expression
;;    using the evaluator with mixed syntactic analysis, rather than
;;    the one that syntactic analysis precedes;
;; 2) the time spent on syntactic analysis by the 'mixed' evaluator
;;    is roughly 49-50%.
;;
;; Time values:
;; - run-time  -> the elapsed run time
;; - gc-time   -> the amount of time spent in the garbage collector
;; - real-time -> the elapsed real time
;;
;; All three times are in ticks.
;; A tick is a unit of time that is unspecified here but
;; can be converted to and from seconds by supplied procedures.
;; 
;; See: evaluator-test.scm


;; Utilities for performance measurements:
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html#Machine-Time
