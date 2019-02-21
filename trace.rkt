; trace.rkt -- Code for temporal traces
;
; A temporal trace is a node whose members include a sequence of timesteps.
; Timesteps are nodes of class 't, linked starting from a hop 'first ->
; 'first-in on the trace node, and 'next -> prev on all the timesteps.

#lang debug at-exp racket

(require "wheel.rkt"
         "model1.rkt"
         "support.rkt"
         "shorthand.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))

(provide (all-defined-out))

; Edges linking these ports define the structure of some "ground" on which we
; might construct some "figure".
(define ground-structure-port-labels
  (set 'members 'member-of 'first 'first-in 'prev 'next))

; (: Graph Node -> (Values Graph Node))
(define (copy-trace g old-trace)
  (let ([(g new-trace) (make-node g 'trace)]
        ;[old-nodes (filter/g g non-tag? (members-of/rec g old-trace))]
        [old-nodes (walk g old-trace node->neighbors/trace)]
        [g (copy-into/as-placeholders g old-trace
                                        new-trace
                                        old-nodes
                                        ground-structure-port-labels)])
    (values g new-trace)))

(define (node->neighbors/trace g node)
  (for/list ([next-node (port->neighbors g `(,node members))]
             #:when (and (non-tag? g next-node)
                         (not (node-is-a? g next-node 'problem))))
    next-node))
