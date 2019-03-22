; model.rkt -- Read/write interface to a FARG model
;
; Essentially an interface to graph.rkt that knows about the FARGish spec.
; Provides all the same functions as graph.rkt, with a few overridden, plus
; some more.

#lang debug at-exp typed/racket

(require debug/repl errortrace)
(require "types.rkt" "typed-wheel.rkt")
(require (only-in "graph.rkt" [make-node g:make-node]
                              [add-edge g:add-edge])
         (except-in "graph.rkt" make-node add-node add-edge)
         "fargish.rkt"
         "fizzle.rkt")
(module+ test (require typed/rackunit))

(provide (all-from-out "graph.rkt")
         (all-defined-out))

;; ======================================================================
;;
;; Functions to make and remove nodes and edges
;;

(: make-node : Graph Attrs -> (Values Graph Node))
(define (make-node g attrs)
  (let ([(g node) (g:make-node g attrs)])
    ; TODO post-make-node
    (values g node)))

(: add-node : Graph Attrs -> Graph)
(define (add-node g attrs)
  (first-value (make-node g attrs)))

(: make-node/in : Graph Node Attrs -> (Values Graph Node))
(define (make-node/in g ctx attrs)
  (let ([(g node) (make-node g attrs)]
        [(g) (add-edge g (member-edge ctx node))])
    (values g node)))

(: add-node/in : Graph Node Attrs -> Graph)
(define (add-node/in g ctx attrs)
  (first-value (make-node/in g ctx attrs)))

(: add-edge (->* (Graph Edge) (EdgeWeight) Graph))
(define (add-edge g edge [weight 1.0])
  (for/fold ([g (g:add-edge g edge weight)])
            ([node (edge->nodes edge)])
    (touch-node g node)))

(: remove-nodes/in : Graph Node -> Graph)
(define (remove-nodes/in g ctx)
  (for/fold ([g g])
            ([node (members-of g ctx)])
    (remove-node g node)))

;TODO Don't make the tag if it's already there
(: make-tag : Graph Attrs (U Node (Listof Node)) -> (Values Graph Node))
(define (make-tag g attrs node/s)
  (let ([(g tag) (make-node g attrs)]
        [nodes (->nodelist node/s)]
        [apply-tag (nodeclass->apply-tag g (class-of g tag))])
    (cond
      [(void? apply-tag) (fizzle:tagclass attrs)]
      [else (let ([g (apply-tag g tag nodes)]
                  [g (for/fold ([g : Graph g])
                               ([ctx (common-ctxs g nodes)])
                       (add-edge g (member-edge ctx tag)))])
              (values g tag))])))

(: add-tag : Graph Attrs (U Node (Listof Node)) -> Graph)
(define (add-tag g tag-attrs node/s)
  (first-value (make-tag g tag-attrs node/s)))

;TODO Move these functions to appropriate places.

(: common-ctxs : Graph (Listof Node) -> (Setof Node))
(define (common-ctxs g nodes)
  (for/fold ([st : (Setof Node) (set)])
            ([node nodes])
    (set-intersect st (list->set (member-of g node)))))

(: ->nodelist : (U Node (Listof Node)) -> (Listof Node))
(define (->nodelist node/s)
  (cond
    [(list? node/s) node/s]
    [else (list node/s)]))

(: nodeclass->apply-tag : (U Graph FARGishSpec) (U Void Symbol)
                          -> (U Void ApplyTag))
(define (nodeclass->apply-tag g nodeclass)
  (cond
    [(void? nodeclass) (void)]
    #:define spec (->spec g)
    [else (hash-ref (FARGishSpec-ht/class->apply-tag spec)
                    nodeclass
                    (const (void)))]))

(: ->spec : (U Graph FARGishSpec) -> FARGishSpec)
(define (->spec g-or-spec)
  (cond
    [(FARGishSpec? g-or-spec) g-or-spec]
    [else (Graph-spec g-or-spec)]))

;; ======================================================================
;;
;; Tracking done after adding a node or edge
;;

(define empty-set : (Setof Node) (set))

(: touch-node : Graph Node -> Graph)
(define (touch-node g node)
  (graph-update-var g 'touched-nodes
    (λ (touched-so-far) (set-add (cast touched-so-far (Setof Node)) node))
    empty-set))

(: clear-touched-nodes : Graph -> Graph)
(define (clear-touched-nodes g)
  (graph-set-var g 'touched-nodes empty-set))

(: record-new-node : Graph Node -> Graph)
(define (record-new-node g node)
  (graph-update-var g 'new-nodes
    (λ (so-far) (set-add (cast so-far (Setof Node)) node))
    empty-set))

(: new-nodes : Graph -> (Setof Node))
(define (new-nodes g)
  (cast (graph-get-var g 'new-nodes empty-set) (Setof Node)))

(: clear-new-nodes : Graph -> Graph)
(define (clear-new-nodes g)
  (graph-set-var g 'new-nodes empty-set))

(: new-node? : Graph Node -> Boolean)
(define (new-node? g node)
  (set-member? (new-nodes g) node))

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

(: member-edge : Node Node -> Edge/List)
(define (member-edge ctx member)
  `((,ctx members) (,member member-of)))

(: walk : Graph Node (-> Graph Node (Listof Node)) -> (Setof Node))
(define (walk g start-node node->next-nodes)
  (let loop ([result empty-set]
             [already-visited (set)]
             [to-visit (list start-node)])
    (cond
      [(null? to-visit) result]
      [else
        (let ([node (car to-visit)]
              [next-nodes (node->next-nodes g node)]
              [already-visited (set-add already-visited node)]
              [old-nodes (set-union already-visited (list->set (cdr to-visit)))]
              [to-visit (append (filter-not (set->pred old-nodes)
                                            next-nodes)
                                (cdr to-visit))])
          (loop (apply set-add* result next-nodes)
                already-visited
                to-visit))])))

;A "stepper" for the 'walk' function.
(: node->neighbors/port-label/ : Port-label -> (-> Graph Node (Listof Node)))
(define (node->neighbors/port-label/ port-label)
  (λ (g node)
    (port->neighbors g `(,node ,port-label))))

; Returns a set of all nodes reachable from start-node's port named port-label,
; and all nodes reachable from those nodes' port named port-label, and so on.
(: follow-port-label/rec : Graph Node Port-label -> (Setof Node))
(define (follow-port-label/rec g start-node port-label)
  (walk g start-node (node->neighbors/port-label/ port-label)))

(: members-of/rec : Graph Node -> (Setof Node))
(define (members-of/rec g node)
  (follow-port-label/rec g node 'members))

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
