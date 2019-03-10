; model.rkt -- Read/write interface to a FARG model
;
; Essentially an interface to graph.rkt that knows about the FARGish spec.
; Provides all the same functions as graph.rkt, with a few overridden, plus
; some more.

#lang debug at-exp typed/racket

(require debug/repl errortrace)
(require "types.rkt" "typed-wheel.rkt")
(require (only-in "graph.rkt" [make-node g:make-node])
         (except-in "graph.rkt" make-node add-node)
         "fargish.rkt")
(module+ test (require typed/rackunit))

(provide (all-from-out "graph.rkt")
         (all-defined-out))

;; ======================================================================
;;
;; Functions to make nodes and edges
;;

(: make-node : Graph Attrs -> (Values Graph Node))
(define (make-node g attrs)
  (let ([(g node) (g:make-node g attrs)])
    (values g node)))

(: add-node : Graph Attrs -> Graph)
(define (add-node g attrs)
  (first-value (make-node g attrs)))

(: make-node/in : Graph Node Attrs -> (Values Graph Node))
(define (make-node/in g ctx attrs)
  (let ([(g node) (make-node g attrs)]
        [(g) (add-edge g `((,ctx members) (,node member-of)))])
    (values g node)))

(: add-node/in : Graph Node Attrs -> Graph)
(define (add-node/in g ctx attrs)
  (first-value (make-node/in g ctx attrs)))

;; ======================================================================
;;
;; Querying individual nodes
;;

(: args-of : Graph Node -> (Listof Any))
(define (args-of g node)
  (cast (get-node-attr g node 'args) (Listof Any)))

(: display-name-of : Graph Node -> Any)
(define (display-name-of g node)
  (get-node-attr g node 'display-name))

(: class-of : Graph (U Node Void) -> (U Void Symbol))
(define (class-of g node)
  (cond
    [(void? node) (void)]
    #:define c (get-node-attr g node 'class)
    [(void? c) (void)]
    [else (cast c Symbol)]))

(: value-of : Graph Node -> Any)
(define (value-of g node)
  (get-node-attr g node 'value))

(: value-of-equal? : Graph Any Node -> Boolean)
(define (value-of-equal? g v node)
  (cond
    [(void? v) #f]
    [else (equal? v (value-of g node))]))

(: has-value? : Graph Node -> Boolean)
(define (has-value? g node)
  (not (void? (value-of g node))))

(: tag? : Graph Node -> (U Any #f))
(define (tag? g node)
  (node-attr? g node 'tag?))

(: node-is-a? : Graph (U Node Void) (U Symbol Void) -> Boolean)
(define (node-is-a? g node ancestor)
  (nodeclass-is-a? (Graph-spec g) (class-of g node) ancestor))

;; ======================================================================
;;
;; Walking the graph
;;

(: members-of : Graph Node -> (Listof Node))
(define (members-of g groupid)
  (port->neighbors g `(,groupid members)))

(: member-of : Graph Node -> (Listof Node))
(define (member-of g node)
  (port->neighbors g `(,node member-of)))

(: member-of? : Graph Node Node -> (U (Listof Node) False))
(define (member-of? g ctx node)
  (member ctx (member-of g node)))

;; ======================================================================
;;
;; Salience
;;

(: salience-of : Graph (U Node Edge Void) -> Flonum)
(define (salience-of g elem)
  (cond
    [(void? elem)
     0.0]
    [(Node? elem)
     (cast (get-node-attr g elem 'salience 0.0) Flonum)]
    [else
     (apply max (map/g g salience-of (edge->nodes elem)))]))

;NEXT Copying a trace, and all its supporting functions
