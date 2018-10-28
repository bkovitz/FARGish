; sigs.rkt -- Signatures for units

#lang racket

(provide (all-defined-out))

(define-signature graph-core^
  (empty-graph
   graph?

   make-node
   remove-node
   add-node
   add-edge
   
   has-node?
   has-edge?
   all-nodes
   all-edges

   port->neighboring-ports
   port->neighbors
   port->neighbor
   port->port-label->nodes 
   port->incident-edges

   node->neighbors
   node->ports
   node->incident-edges

   define/g
   gdo))

(define-signature graph-node-attrs^
  (get-node-attr
   set-node-attr
   update-node-attr
   get-node-attrs
   union-node-attrs
   node-attr?
   graph-edge-weight))

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
