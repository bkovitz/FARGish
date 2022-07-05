; model.ss -- Simplest possible Scheme implementation of
;             canvases-and-painters

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
      (let** ([I (eval-as 'det-addr source)]
              [J (eval-as 'det-addr target)]
              [F (eval-as 'func func)]
              [V (apply-func F (eval-as 'value-at I))])
        (paint J V))]))

(define (eval-as env what-as x)
  (cond
    [(eq? what-as 'det-addr)
     (

; What are all the possibilities for x?
; If x is a number, what are all the possibilities for
; supplying the container?

; How do we store containers and evaluators in the env?

(define (default-eval-as-det-addr env x)
  (cond x
    [(det-addr? x) x]
    [(integer? x) (det-addr (default-container env) x)]
