; graph.rkt -- Data structure for port graphs

#lang debug at-exp typed/racket

(require "types.rkt"
         "id-set.rkt")

(define-type Attrs (Hashof Any Any))

(struct Graph ([ht-node->attrs : (Hashof Node Attrs)]
               [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
               [edges : (Hashof Edge/Set EdgeWeight)]
               [id-set : IdSet]
               ;[stacks : ??]
               [vars : (Hashof Symbol Any)]))

(define empty-graph
  (Graph (hash) (hash) (hash) empty-id-set (hash)))

;(define-struct/exec Gfunc () 

;(: make-node (Graph Attrs -> (Values Graph Node)))
;(define (make-node g attrs)
;  (let*-values ([(attrs name) (ensure-node-name g attrs)]

(: ensure-node-name : Graph Attrs -> (Values Attrs Name))
(define (ensure-node-name g attrs)
  (: set-name-and-return : Name -> (Values Attrs Name))
  (define (set-name-and-return name)
    (values (hash-set attrs 'name name) name))
  (cond
    [(hash-ref attrs 'name #f)
     => (λ (name) (values attrs name))]
    [(hash-ref attrs 'value #f)
     => set-name-and-return]
    [(hash-ref attrs 'class #f)
     => set-name-and-return]
    [else (set-name-and-return 'UNKNOWN)]))
