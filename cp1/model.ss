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

; The default (simplest) way to map an addr to a det-addr.
(define (eval-as/det-addr/default env x)
  (cond x
    [(det-addr? x) x]
    [(integer? x) (det-addr (default-container env) x)]
    [(char? x) (matching-cells (default-container env) x)]
    [(painter-template? x) (matching-painters (default-soup env) x)]
    [(soup? x) x]
    [else fizzle(x)]))

(define (default-container env)
  (env-main-canvas env))  ; No flexibility in this version

(define (default-soup env)
  (env-working-soup env))  ; No flexibility in this version

; TODO
; det-addr?
; det-addr  (ctor)
; matching-cells
; matching-painters
; soup?
; fizzle


    
