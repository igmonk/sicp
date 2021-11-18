;; Exercise 3.20
;;
;; Draw environment diagrams to illustrate the evaluation of the sequence
;; of expressions
;;
;; (define x (cons 1 2))
;; (define z (cons x x))
;; (set-car! (cdr z) 17)
;; (car x) ; 17
;;
;; using the procedural implementation of pairs given above
;; (Compare exercise 3.11.)
;;
;; See workbook.scm


;; Result of defining 'cons' in the global environment:
;;
;; ----------------------------------------------------------------------------------
;;
;;  Global
;;  Env              cons---↓
;;                          |
;; -------------------------|--------------------------------------------------------
;;                          |    ↑
;;                          ↓    |
;;                        |x|x|--↑
;;                         |
;;                         ↓
;;               parameters: x,y
;;               body: (define set-x! ...)
;;                     (define set-y! ...)
;;                     (define dispatch ...)
;;                     dispatch


;; Result of evaluating (define x (cons 1 2))
;;
;; ----------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env              cons---↓                           x---↓
;;                          |                               |
;; -------------------------|-------------------------------|------------------------------------
;;                          |    ↑                          |             ↑
;;                          ↓    |                          |             |______________________
;;                        |x|x|--↑                          |           E1|x:1
;;                         |                                |             |y:2
;;                         |                                |             |set-x!:
;;                         ↓                                |             |set-y!:
;;               parameters: x,y                            |             |dispatch:-----↓     ↑
;;               body: (define set-x! ...)                  |             |______________|_____|_
;;                     (define set-y! ...)                  |             |              |     |
;;                     (define dispatch ...)                ↓             |            |x|x|---↑
;;                     dispatch                           |x|x|-----------↑             ↓
;;                                                         ↓                  parameters:m
;;                                               parameters:m                 body: (cond ...)
;;                                               body: (cond ...)


;; Result of evaluating (define z (cons x x))
;;
;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env              cons---↓                           x---↓                                              z---↓
;;                          |                               |                                                  |
;; -------------------------|-------------------------------|--------------------------------------------------|-----------------------------------
;;                          |    ↑                          |             ↑                                    |            ↑
;;                          ↓    |                          |             |______________________              |            |______________________
;;                        |x|x|--↑                          |           E1|x:1                                 |          E2|x:x
;;                         |                                |             |y:2                                 |            |y:x
;;                         |                                |             |set-x!:                             |            |set-x!:
;;                         ↓                                |             |set-y!:                             |            |set-y!:
;;               parameters: x,y                            |             |dispatch:-----↓     ↑               |            |dispatch:-----↓     ↑
;;               body: (define set-x! ...)                  |             |______________|_____|_              |            |______________|_____|_
;;                     (define set-y! ...)                  |             |              |     |               |            |              |     |
;;                     (define dispatch ...)                ↓             |            |x|x|---↑               ↓            |            |x|x|---↑
;;                     dispatch                           |x|x|-----------↑             ↓                    |x|x|----------↑             ↓
;;                                                         ↓                  parameters:m                    ↓                 parameters:m
;;                                               parameters:m                 body: (cond ...)      parameters:m                body: (cond ...)
;;                                               body: (cond ...)                                   body: (cond ...)


;; Result of evaluating (set-car! (cdr z) 17)
;;
;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;
;;  Global
;;  Env              cons---↓                           x---↓                                              z---↓
;;                          |                               |                                                  |
;; -------------------------|-------------------------------|--------------------------------------------------|-----------------------------------
;;                          |    ↑                          |             ↑                                    |            ↑
;;                          ↓    |                          |             |______________________              |            |______________________
;;                        |x|x|--↑                          |           E1|x:17                                |          E2|x:x
;;                         |                                |             |y:2                                 |            |y:x
;;                         |                                |             |set-x!:                             |            |set-x!:
;;                         ↓                                |             |set-y!:                             |            |set-y!:
;;               parameters: x,y                            |             |dispatch:-----↓     ↑               |            |dispatch:-----↓     ↑
;;               body: (define set-x! ...)                  |             |______________|_____|_              |            |______________|_____|_
;;                     (define set-y! ...)                  |             |              |     |               |            |              |     |
;;                     (define dispatch ...)                ↓             |            |x|x|---↑               ↓            |            |x|x|---↑
;;                     dispatch                           |x|x|-----------↑             ↓                    |x|x|----------↑             ↓
;;                                                         ↓                  parameters:m                    ↓                 parameters:m
;;                                               parameters:m                 body: (cond ...)      parameters:m                body: (cond ...)
;;                                               body: (cond ...)                                   body: (cond ...)
