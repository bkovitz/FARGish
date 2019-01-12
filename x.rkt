#lang debug at-exp racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require racket/hash)
(require "wheel.rkt")
(require rackunit)

(require (prefix-in m: "model1.rkt")
         "shorthand.rkt"
         (prefix-in e: "equation.rkt")
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))


; Makes an acceptability function that implements a desideratum.
; Returns (g node -> acceptability-result)
(define (mk-acceptability desideratum)
  (match desideratum
    ['() (const (void))]
    [`(of-class ,class)
      (mk-acceptability/of-class class)]
    [`(in-role ,port-label)
      (mk-acceptability/in-role port-label)]
    [`(and ,desiderata ...)
      (mk-chain-result-by safe-* (map mk-acceptability desiderata))]

;    [`((of-class ,class) . ,more)
;      (curry (mk-acceptability/of-class class)
;             (->acc more))]
    
    ))


; combine-results : (acceptability-result ... -> acceptability-result)
; desideratum-funcs : (Listof (g node -> acceptability-result))
; Returns (g node -> acceptability-result)
(define (mk-chain-result-by combine-results desideratum-funcs)
  (λ (g node)
    (let loop ([desideratum-funcs desideratum-funcs]
               [result-so-far (void)])
      (cond
        [(null? desideratum-funcs)
         (combine-results result-so-far)]
        [else
         (let* ([dfunc (car desideratum-funcs)]
                [r (dfunc g node)])
           (cond
             [(eq? 'reject r) 'reject]
             [else (loop (cdr desideratum-funcs)
                         (combine-results result-so-far r))]))]))))


;(define (acceptability/final-result g result-so-far node)
;  result-so-far)

;(define (acceptability/generic node-measure sk g result-so-far node)
;  (let ([measure (node-measure g node)])
;    (cond
;      [(eq? 'reject measure)
;       'reject]
;      [else (sk g (safe-* result-so-far measure) node)])))

(define (mk-all-or-nothing pred? . final-args)
  (λ (g node)
    (if (apply pred? g node final-args)
      1.0
      'reject)))

(define (mk-acceptability/of-class class)
  (mk-all-or-nothing m:node-is-a? class))

(define (mk-acceptability/in-role port-label)
  (mk-all-or-nothing g:has-neighbor-from-port-label? port-label))


;;;;;;;;;;;;;;;;;;;;

(define spec
  (farg-model-spec
    (nodeclass (number n)
      (name n)
      (value n))
    (nodeclass (equation nm)
      (is-a 'ctx)
      (name nm))
    (nodeclass +)))

(define g (m:make-empty-graph spec))
(gdo e:make-equation 5 '(+ 2 3))

(define ac (mk-acceptability '(of-class number)))
(define ac2 (mk-acceptability '(in-role result)))
(define ac3 (mk-acceptability '(and (of-class number) (in-role result))))
