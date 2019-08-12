; sigs.rkt -- Signatures for units

#lang racket

(provide (all-defined-out))

(define-signature graph-core^
  (make-node  ; g attrs -> g nodeid
   remove-node
   add-node
   add-edge
   remove-edge
   
   has-node?
   has-edge?
   graph-edge-weight
   all-nodes
   all-edges

   port->neighboring-ports
   port->neighbors
   port->neighbor
   port-neighbor?
   port->port-label->nodes 
   port->incident-edges

   node->ports
   node->incident-hops
   node->neighbors

   ;define/g
   ;gdo
   ))

(define-signature simple-graph-struct^
  ((struct graph (ht-node->attrs
                 ht-port->neighboring-ports
                 edges
                 id-set
                 stacks  ; dict of temp vars for do-graph-edits
                 vars    ; hash-table of vars: name -> value
                 spec))
   empty-graph))

(define-signature graph-name^
  (ensure-node-name)) ; g attrs -> attrs name

(define-signature graph-node-attrs^
  (get-node-attr
   set-node-attr
   update-node-attr
   get-node-attrs
   union-node-attrs
   node-attr?))

(define-signature graph-vars^
  (graph-set-var
   graph-get-var
   graph-update-var
   graph-push-var
   graph-push-and-set-var
   graph-pop-var))

(define-signature graph-print^
  (pr-node
   pr-graph
   pr-group))

;(define-signature graph-class^
;  (class-of
;   find-nodes-of-class
