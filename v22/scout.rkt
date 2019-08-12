; scout.rkt -- Scout nodes: agent nodes that search the FARG graph for something

#lang debug at-exp racket

(require "wheel.rkt" "observe.rkt")

(require racket/hash)
(require racket/generic)
(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

(provide step)

(define (step gather-candidates select-results g initial-ht-candidates)
  (define ht-candidates (gather-candidates g initial-ht-candidates))
  (define results (select-results g ht-candidates))
  (values ht-candidates results))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "model1.rkt"
         "spreading-activation.rkt"
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))

(require "shorthand.rkt"
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass))
(require "equation.rkt")

(define spec
  (farg-model-spec
    (nodeclass (number n)
      (name n)
      (value n))
    (nodeclass (equation nm)
      (is-a 'ctx)
      (name nm))
    (nodeclass +)))

(define g (make-empty-graph spec))
(gdo make-equation 5 '(+ 2 3))

(define (gather-candidates g ht-candidates)
  (hash '(+ 2 3) 0.8))

(define (make-select-results desideratum)
  (define (select-results g ht-candidates)
    (...))
  select-results)

(define sr (make-select-results
             '(node (of-class equation)
                    (has-member (in-role result)
                                (match-value 5 (stringency 1.0)))
                    (has-member (in-role operands)
                                (value 2))
                    (has-member (in-role operands)
                                (value 3)))))
