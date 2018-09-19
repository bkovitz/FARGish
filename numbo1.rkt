#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         describe "graph.rkt")

;(define (build-from-primitive-archetype g archetype bdx-from-archetype)
;  ...)

;(define (finish-archetype-instantiation g archetype ctx bdx-from-archetype)
  ; 1. start scouts for the unbound nodes
  ; Is that all?

;(define g (make-graph 4))

;Need some sort ;of expr->graph func.
(define g (apply make-graph '((:group archetype
                                 4 5 + 9
                                 (:edge (4 result) (+ operands))
                                 (:edge (5 result) (+ operands))
                                 (:edge (+ result) (9 source))))))

(pr-graph g)
