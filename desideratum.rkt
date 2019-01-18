; desideratum.rkt -- Desiderata: things to search for and build

#lang debug at-exp racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require racket/hash)
(require "wheel.rkt")
(require expect/rackunit (only-in rackunit test-case))

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


; An acceptability-result is either a number or 'reject.

; Makes an acceptability function that implements a desideratum.
; Returns (g node -> acceptability-result)
(define (mk-acceptability desideratum)
  (match desideratum
    ['() (const (void))]
    [`(of-class ,class)
      (mk-acceptability/of-class class)]
    [`(in-role ,port-label)
      (mk-acceptability/in-role port-label)]
    [`(value ,v)
      (mk-acceptability/value v 1.0)]
    [`(value ,v (stringency ,s))
      (mk-acceptability/value v s)]
    [`(and ,desiderata ...)
      (mk-chain-result-by safe-* (map mk-acceptability desiderata))]))

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

(define (mk-all-or-nothing pred? . final-args)
  (λ (g node)
    (if (apply pred? g node final-args)
      1.0
      'reject)))

(define (mk-acceptability/of-class class)
  (mk-all-or-nothing m:node-is-a? class))

(define (mk-acceptability/in-role port-label)
  (mk-all-or-nothing g:has-neighbor-from-port-label? port-label))

; Returns a function that maps a number into a logarithmic space, where
; numbers near the center (the 'scale') are mapped further apart than
; numbers further away from the center.
;
; The function is a bit of a hack. The hyperbola defined in the s function
; should be rotated a bit. The logspace function will give bad results when
; x <= -scale.
(define (mk-logspace scale)
  (define (s x) (/ scale         ; the "slope" term in the logistic func
                   (+ x scale))) ; (not really slope except where x == scale)
  (λ (x)
    (/ 1.0
       (+ 1.0 (exp (* (- (s x))
                      (- x scale)))))))

(define (mk-proximity-measure scale stringency)
  (let* ([logspace (mk-logspace scale)]
         [stringency (clamp 0.0 1.0 stringency)]
         [stringency-factor (- (/ (+ (expt (- 1.0 stringency) 2)
                                     0.0001))
                               1.0)])
    (λ (x1 x2)
      (let ([logdistance (abs (- (logspace x1) (logspace x2)))])
        (exp (- (expt (* logdistance stringency-factor) 2)))))))
    

(define (mk-acceptability/value target-value stringency)
  (let ([proximity-measure (mk-proximity-measure target-value stringency)])
    (λ (g node)
      (let ([actual-value (m:value-of g node)])
        (cond
          [(void? actual-value)
           (void)]
          [(not (number? actual-value))
           'reject]
          [else
            (proximity-measure actual-value target-value)])))))


;; ======================================================================
;;
;; Unit tests
;;

(module+ test
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

  (test-case "desideratum basics"
    (define des-number (mk-acceptability '(of-class number)))
    (define des-result (mk-acceptability '(in-role result)))
    (define des-both (mk-acceptability
                       '(and (of-class number) (in-role result))))
    (define des-3 (mk-acceptability '(value 3)))
    (define des-3-inexact (mk-acceptability '(value 3 (stringency 0.5))))
    (check-equal? (des-number g 5) 1.0)
    (check-equal? (des-number g '+) 'reject)
    (check-equal? (des-result g 5) 1.0)
    (check-equal? (des-result g 2) 'reject)
    (check-equal? (des-both g 5) 1.0)
    (check-equal? (des-both g 2) 'reject)
    (check-equal? (des-3 g 3) 1.0)
    (check-equal? (des-3 g 2) 0.0)
    (let ([d3 (des-3-inexact g 3)]
          [d2 (des-3-inexact g 2)]
          [d5 (des-3-inexact g 5)]
          [d+ (des-3-inexact g '+)])
      (check-equal? d3 1.0)
      (check-true (< 0.0 d5 d2 d3))
      (check-pred void? d+))))
