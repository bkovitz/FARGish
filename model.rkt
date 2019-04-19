; model.rkt -- Read/write interface to a FARG model
;
; Essentially an interface to graph.rkt that knows about the FARGish spec.
; Provides all the same functions as graph.rkt, with a few overridden, plus
; some more.

#lang debug at-exp typed/racket
(require typed/debug/report)
(require debug/repl errortrace)

(require typed/json)
(require "types.rkt" "typed-wheel.rkt")
(require (only-in "graph.rkt" [make-node g:make-node]
                              [add-edge g:add-edge])
         (except-in "graph.rkt" make-node add-node add-edge)
         "fargish.rkt"
         "fizzle.rkt")
(module+ test (require typed/rackunit))

(provide (all-from-out "graph.rkt")
         (except-out (all-defined-out)
           return/ d3-derived-attrs add-d3-derived-attrs add-attr))

;TODO Global constants should be stored as variables in the graph.
(define salience-decay 0.8)

;; ======================================================================
;;
;; Functions to make and remove nodes and edges
;;

(: make-node : Graph (U Attrs Symbol) -> (Values Graph Node))
(define (make-node g attrs)
  (let ([attrs (if (symbol? attrs)
                 (hash 'class attrs)
                 attrs)]
        [(g node) (g:make-node g attrs)])
    ; TODO post-make-node
    (values g node)))

(: add-node : Graph Attrs -> Graph)
(define (add-node g attrs)
  (first-value (make-node g attrs)))

(: add-nodes : Graph Attrs * -> Graph)
(define (add-nodes g . attrss)
  (for/fold ([g g])
            ([attrs : Attrs attrss])
    (add-node g attrs)))

(: make-node/in : Graph Node (U Attrs Symbol) -> (Values Graph Node))
(define (make-node/in g ctx attrs)
  (let ([(g node) (make-node g attrs)]
        [(g) (add-edge g (member-edge ctx node))])
    (values g node)))

(: add-node/in : Graph Node Attrs -> Graph)
(define (add-node/in g ctx attrs)
  (first-value (make-node/in g ctx attrs)))

(: add-nodes/in : Graph Node Attrs * -> Graph)
(define (add-nodes/in g ctx . attrss)
  (for/fold ([g g])
            ([attrs : Attrs attrss])
    (add-node/in g ctx attrs)))

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

(: make-member-of : Graph MaybeNode MaybeNode * -> Graph)
(define (make-member-of g ctx . nodes)
  (cond
    [(void? ctx) g]
    #:define nodes (->nodelist nodes)
    [else (for/fold ([g g])
                    ([node nodes])
            (add-edge g `((,ctx members) (,node member-of))))]))

;; ======================================================================
;;
;; Path
;;

(: path->node : Graph Node Port-label * -> (U Void Node))
(define (path->node g node . port-labels)
  (let loop ([node node] [port-labels port-labels])
    (cond
      [(null? port-labels) node]
      #:define next-node (port->neighbor g `(,node ,(car port-labels)))
      [(void? next-node) (void)]
      [else (loop next-node (cdr port-labels))])))

;; ======================================================================
;;
;; Tagging
;;

;TODO Don't make the tag if it's already there
(: make-tag : Graph (U Attrs Symbol) (U Node (Listof Node))
              -> (Values Graph Node))
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

(: add-tag : Graph (U Attrs Symbol) (U Node (Listof Node)) -> Graph)
(define (add-tag g tag-attrs node/s)
  (first-value (make-tag g tag-attrs node/s)))

(: nodeclass->apply-tag : (U Graph FARGishSpec) (U Void Symbol)
                          -> (U Void ApplyTag))
(define (nodeclass->apply-tag g nodeclass)
  (cond
    [(void? nodeclass) (void)]
    #:define spec (->spec g)
    [else (hash-ref (FARGishSpec-ht/class->apply-tag spec)
                    nodeclass
                    (const (void)))]))

; Is tag tagging all of node/s?
(: is-tagging? : Graph (U Node Void) MaybeNodes -> Boolean)
(define (is-tagging? g tag node/s)
  (cond
    [(any-void? node/s) #f]
    [(void? tag) #f]
    #:define nodes (->nodelist node/s)
    [(null? nodes) #f]
    [else
      (andmap (is-tagging?1/ g tag) nodes)]))

; Makes helper closure for is-tagging?.
(: is-tagging?1/ : Graph Node -> (Node -> Boolean))
(define (is-tagging?1/ g tag)
  (let ([spec (Graph-spec g)]
        [tagclass (class-of g tag)]
        [taggee-infos (hash-ref (FARGishSpec-ht/class->taggee-infos spec)
                                  tagclass
                                  (const '()))])
    (λ ([node : Node]) : Boolean
      (for/or ([info taggee-infos])
        (let ([from-label (TaggeeInfo-from-port-label info)]
              [to-label (TaggeeInfo-to-port-label info)])
          (has-edge? g `((,tag ,from-label) (,node ,to-label))))))))

(: has-tag?/1 : Graph Symbol Node -> Boolean)
(define (has-tag?/1 g tagclass node)
  (let ([spec (Graph-spec g)]
        [taggee-infos (hash-ref (FARGishSpec-ht/class->taggee-infos spec)
                                  tagclass
                                  (const '()))])
    (for/or ([info taggee-infos])
      (let ([from-label (TaggeeInfo-from-port-label info)]
            [to-label (TaggeeInfo-to-port-label info)])
        (for/or ([neighbor (port->neighbors g `(,node ,to-label))]) : Boolean
          (and (node-is-a? g neighbor tagclass)
               (has-edge? g `((,neighbor ,from-label) (,node ,to-label)))))))))

(: has-tag? : Graph Symbol MaybeNodes -> Boolean)
(define (has-tag? g tagclass node/s)
  (cond
    [(Node? node/s) (has-tag?/1 g tagclass node/s)]
    [(null? node/s) #f]
    [(any-void? node/s) #f]
    #:define nodes (->nodelist node/s)
    #:define candidate-tags (filter (node-is-a?/ g tagclass)
                                    (set->list (common-neighbors g node/s)))
    [else (let ([is? (λ ([tag : Node]) : Boolean
                       (is-tagging? g tag node/s))])
            (ormap is? candidate-tags))]))

; Returns #t iff the nodes meet the TaggeeInfo criteria to be tagged by
; tagclass.
(: could-apply-to? : Graph Symbol MaybeNodes -> Boolean)
(define (could-apply-to? g tagclass maybe-nodes)
  (let ([infos (class->taggee-infos g tagclass)])
    (cond
      [(void? infos) #f]  ; BAD? Should we fizzle:tagclass ?
      [else
        (for/and ([node (->nodelist maybe-nodes)]
                  [info infos])
                  ;TODO WRONG: Need to pad the shorter list
          (let ([nodeclass (class-of g node)]
                [of-classes (TaggeeInfo-of-classes info)])
            (or (null? of-classes)
                (for/or ([of-class of-classes]) : Boolean
                  (nodeclass-is-a? g nodeclass of-class)))))])))

(: tagclass-applies-to? : Graph (U Symbol Attrs) MaybeNodes -> (U Any #f))
(define (tagclass-applies-to? g class-or-attrs node/s)
  (let ([tagclass : Symbol
          (cond
            [(symbol? class-or-attrs) class-or-attrs]
            #:define cl (hash-ref class-or-attrs 'class (const (void)))
            [(void? cl) (fizzle:no-class class-or-attrs)]
            [(symbol? cl) cl]
            [else (fizzle:class-not-symbol class-or-attrs)])])
    (cond
      [(not (could-apply-to? g tagclass node/s)) #f]
      #:define cfunc (class->condition g tagclass)
      [(void? cfunc) #f]  ; BAD? Should we fizzle:tagclass ?
      #:define ht/taggees (nodes->ht/taggees g tagclass node/s)
      [(void? ht/taggees) #f]
      [else (cfunc g (void) ht/taggees)])))

; Prepares third argument to a tagclass's condition function: a hash table
; mapping taggee names to nodes.
(: nodes->ht/taggees : Graph Symbol MaybeNodes
                       -> (Maybe (Hashof Symbol MaybeNodes)))
(define (nodes->ht/taggees g tagclass node/s)
  (cond
    [(void? node/s) (hash)]
    [(null? node/s) (hash)]
    #:define node/s (if (Node? node/s) (list node/s) node/s)
    #:define infos (class->taggee-infos g tagclass)
    [(void? infos) (void)]
    [(< (length infos) (length node/s)) (void)]
    [else (for/fold ([ht : (Hashof Symbol MaybeNodes) (hash)])
                    ([maybe-node (->nodelist node/s)] [info infos])
            (cond
              [(void? maybe-node) ht]
              [else (hash-set ht (TaggeeInfo-name info) maybe-node)]))]))

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

(: touched-nodes : Graph -> (Setof Node))
(define (touched-nodes g)
  (cast (graph-get-var g 'touched-nodes empty-set) (Setof Node)))

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

(define attrs-of get-node-attrs)

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

(: value-of : Graph (Maybe Node) -> (Maybe Any))
(define (value-of g node)
  (get-node-attr g node 'value))

(: value-of-equal? : Graph Any Node -> Boolean)
(define (value-of-equal? g v node)
  (cond
    [(void? v) #f]
    [else (equal? v (value-of g node))]))

(: has-value? : Graph MaybeNode -> Boolean)
(define (has-value? g node)
  (not (void? (value-of g node))))

(: tag? : Graph Node -> (U Any #f))
(define (tag? g node)
  (node-attr? g node 'tag?))

(: node-is-a? : Graph (U Node Void) (U Symbol Void) -> Boolean)
(define (node-is-a? g node ancestor)
  (nodeclass-is-a? (Graph-spec g) (class-of g node) ancestor))

(: node-is-a?/ : Graph Symbol -> (-> (U Node Void) Boolean))
(define (node-is-a?/ g class)
  (λ (node)
    (node-is-a? g node class)))

;; ======================================================================
;;
;; Walking the graph
;;

(: common-neighbors : Graph MaybeNodes -> (Setof Node))
(define (common-neighbors g node/s)
  (apply set-union* (map (curry node->neighbors g) (->nodelist node/s))))

(: members-of : Graph Node -> (Listof Node))
(define (members-of g groupid)
  (port->neighbors g `(,groupid members)))

(: member-of : Graph Node -> (Listof Node))
(define (member-of g node)
  (port->neighbors g `(,node member-of)))

(: member-of? : Graph (U Node Void) (U Node Void) -> (U (Listof Node) False))
(define (member-of? g ctx node)
  (cond
    [(void? node) #f]
    [(void? ctx) #f]
    [else (member ctx (member-of g node))]))

(: member-edge : Node Node -> Edge/List)
(define (member-edge ctx member)
  `((,ctx members) (,member member-of)))

; members-of/rec is simpler
;(: members-of/recursive : Graph Node [#:in-progress (Setof Node)]
;                          -> (Setof Node))
;(define (members-of/recursive g node #:in-progress [in-progress empty-set])
;  (cond
;    #:define in-progress (set-add in-progress node)
;    #:define ms (members-of g node)
;    [(null? ms) empty-set]
;    [else (for/fold ([result (list->set ms)])
;                    ([m ms])
;            (set-union result 
;                       (members-of/recursive g m #:in-progress in-progress)))]))

(: container? : Graph Node -> Boolean)
(define (container? g node)
  (not (null? (members-of g node))))

(: common-ctxs : Graph (Listof Node) -> (Setof Node))
(define (common-ctxs g nodes)
  (for/fold ([st : (Setof Node) (set)])
            ([node nodes])
    (set-intersect st (list->set (member-of g node)))))

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

(: g->node->neighbors : Graph -> (-> Node (Setof Node)))
(define (g->node->neighbors g)
  (λ (node) (node->neighbors g node)))

(: g->node->salience : Graph -> (-> Node Salience))
(define (g->node->salience g)
  (λ (node) (salience-of g node)))

(: nearby-nodes
   (->* [Graph Node] [#:num-hops Integer #:filter (U #f (Node -> (U Any #f)))]
        (Setof Node)))
(define (nearby-nodes g node #:num-hops [num-hops 2] #:filter [fi #f])
  (if (zero? num-hops)
    empty-set
    (let ([node->neighbors (g->node->neighbors g)]
          [nodes->neighbors (λ ([st : (Setof Node)]) : (Setof Node)
                              (for/fold ([result empty-set])
                                        ([node (in-set st)])
                                (set-union result (node->neighbors node))))]
          [accumulate (if fi
                        (λ ([result : (Setof Node)] [nodes : (Setof Node)])
                          (for/fold ([result : (Setof Node) result])
                                    ([node nodes]
                                     #:when (fi node))
                            (set-add result node)))
                        set-union)])
      (let loop ([result (accumulate empty-set (node->neighbors node))]
                 [to-do (node->neighbors node)]
                 [done (set node)]
                 [num-hops (sub1 num-hops)])
        (cond
          [(zero? num-hops) result]
          [else (loop (accumulate result (nodes->neighbors to-do))
                      (set-subtract (nodes->neighbors to-do) to-do done)
                      (set-union done to-do)
                      (sub1 num-hops))])))))

;; ======================================================================
;;
;; Convenience functions for overloaded arguments
;;

(: ->nodelist : (U Node Void (Listof (U Node Void))) -> (Listof Node))
(define (->nodelist node/s)
  (cond
    [(void? node/s) '()]
    [(Node? node/s) (list node/s)]
    [else (non-voids node/s)]))

(: ->nodes : (U Node Void (Listof Node) (Setof Node))
   -> (U (Listof Node) (Setof Node)))
(define (->nodes node/s)
  (cond
    [(void? node/s) '()]
    [(Node? node/s) (list node/s)]
    [else node/s]))

; Forces result to be one Node (or Void) even if name corresponds to
; many nodes in the hash table.
(: ht->node : (Hashof Symbol (U Node Void (Listof (U Node Void))))
              Symbol
              -> (U Node Void))
(define (ht->node ht name)
  (let ([x (hash-ref ht name (const (void)))])
    (cond
      [(void? x) x]
      [(Node? x) x]
      [else (first-non-void x)])))

(: ht->node/s : (Hashof Symbol (U Node Void (Listof (U Node Void))))
                Symbol
                -> (U (Listof Node) Void))
(define (ht->node/s ht name)
  (let ([x (hash-ref ht name (const (void)))])
    (cond
      [(Node? x) (list x)]
      [else (non-voids x)])))

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

(: set-salience-of : Graph (U Nodes Void) Flonum -> Graph)
(define (set-salience-of g node s)
  (cond
    [(void? node) g]
    [(Node? node) (set-node-attr g node 'salience s)]
    [else (for/fold ([g g])
                    ([n node])
            (set-node-attr g n 'salience s))]))

(: boost-salience-of : Graph Node -> Graph)
(define (boost-salience-of g node)
  (update-node-attr g node 'salience
    (cast (curry+ 1.0) (Any -> Any))
    (const 0.0)))

(: boost-salience-of-touched-nodes : Graph -> Graph)
(define (boost-salience-of-touched-nodes g)
  (for/fold ([g g])
            ([node (touched-nodes g)])
    (boost-salience-of g node)))

(: reduce-salience-of : Graph Node -> Graph)
(define (reduce-salience-of g node)
  (update-node-attr g node 'salience
    (cast (curry * 0.5) (Any -> Any))
    (const 0.0)))

(: decay-saliences-in : Graph Node -> Graph)
(define (decay-saliences-in g ctx)
  (for/fold ([g g])
            ([node (in-set (members-of/rec g ctx))])
    (decay-salience-of g node)))

(: decay-salience-of : Graph Node -> Graph)
(define (decay-salience-of g node)
  (update-node-attr g node 'salience
    (cast (curry * salience-decay) (Any -> Any))
    (const 0.0)))

(: decay-salience/all : Graph -> Graph)
(define (decay-salience/all g)
  (for/fold ([g g])
            ([node (all-nodes g)])
    (decay-salience-of g node)))

; Returns Node or void if no node found. If #:filter is #f, then no nodes
; are filtered from the nearby nodes.
(: choose-nearby-node-by-salience
   (->* [Graph Node] [#:num-hops Integer #:filter (U #f (-> Node (U Any #f)))]
        (U Node Void)))
(define (choose-nearby-node-by-salience g node #:num-hops [num-hops 2]
                                               #:filter [fi #f])
  (let ([nodes (set->list (nearby-nodes g node #:num-hops num-hops
                                               #:filter fi))])
    (cond
      [(null? nodes) (void)]
      [else (weighted-choice-by (g->node->salience g) nodes)])))

; Randomly chooses member of ctx.
; Returns Node or void is no node found.
; If #:filter is #f, then no nodes are filtered from the members of ctx.
(: choose-node-by-salience/in 
   (->* [Graph (U Node Void)] [#:filter (U #f (-> Node Boolean))]
        (U Node Void)))
(define (choose-node-by-salience/in g ctx #:filter [fi #f])
  (cond
    [(void? ctx) (void)]
    [else (weighted-choice-by (g->node->salience g)
                              (if fi
                                (filter fi (members-of g ctx))
                                (members-of g ctx)))]))

(: display-salience (->* [Graph Node] [(U Salience Void)] Void))
(define (display-salience g node [salience (void)])
  (let ([salience (if (void? salience) (salience-of g node) salience)]
        [s-node (~a (~a node #:min-width 15))]
        [s-salience (~r salience #:precision '(= 3))])
    (displayln (string-append s-node " " s-salience))))

(: pr-saliences : Graph -> Void)
(define (pr-saliences g)
  (for ([node (members-of g 'ws)])
    (display-salience g node)))

(: saliences-ht : Graph -> (Hashof Node Salience))
(define (saliences-ht g)
  (for/hash ([node (members-of g 'ws)]) : (Hashof Node Salience)
    (values node (salience-of g node))))

;; ======================================================================
;;
;; Global graph vars
;;

(: current-t : Graph -> Integer)
(define (current-t g)
  (cast (graph-get-var g 't 0) Integer))

(: bump-t : Graph -> Graph)
(define (bump-t g)
  (graph-set-var g 't (add1 (current-t g))))

;; ======================================================================
;;
;; Copying bunches of nodes
;;

(: copy-into : Graph Node Node (U (Listof Node) (Setof Node)) (Setof Port-label)
               (-> Graph Node (Values Graph Node))
               -> Graph)
(define (copy-into g
                   orig-ctx             ; Link this...
                   new-ctx              ; ...to this
                   orig-nodes       ; and propagate links among copies of these
                   relevant-port-labels ; that involve these port-labels.
                   copy-node)           ; Node-copying function
  (: make-nodes : -> (Values Graph (Hashof Node Node)))
  (define (make-nodes)
    (for/fold ([g g] [nodemap : (Hashof Node Node) (hash orig-ctx new-ctx)])
              ([orig-node orig-nodes])
      (let ([(g new-node) (copy-node g orig-node)])
        (values g (hash-set nodemap orig-node new-node)))))

  (: make-edges : Graph (Hashof Node Node) -> Graph)
  (define (make-edges g nodemap)
    (let ([orig-node? (hash->pred nodemap)]
          [relevant-port-label? (set->pred relevant-port-labels)])
      (for*/fold ([g : Graph g])
                 ([(orig-node new-node) nodemap]
                  [port-label relevant-port-labels]
                  [orig-hop : Hop
                    (port->incident-hops g `(,orig-node ,port-label))])
        (cond
          #:define neighbor-label (other-port-label orig-hop)
          [(not (relevant-port-label? neighbor-label))
           g]
          #:define orig-neighbor (other-node orig-hop)
          [(not (orig-node? orig-neighbor))
           g]
          #:define new-neighbor (hash-ref nodemap orig-neighbor)
          [else (add-edge g `((,new-node ,port-label)
                              (,new-neighbor ,neighbor-label)))]))))
  (let ([(g nodemap) (make-nodes)]
        [g (make-edges g nodemap)])
    g))

(: copy-as-placeholder-or-t : Graph Node -> (Values Graph Node))
(define (copy-as-placeholder-or-t g orig-node)
  (let ([(g new-node) (cond
                        [(node-is-a? g orig-node 't)
                         (make-node g (cast (attrs-of g orig-node) Attrs))]
                        [else (make-placeholder g
                                (cast (class-of g orig-node) Symbol))])]
        [g (mark-copying g orig-node new-node)])
    (values g new-node)))

(: copy-into/as-placeholders :
   Graph Node Node (U (Listof Node) (Setof Node)) (Setof Port-label)
   -> Graph)
(define (copy-into/as-placeholders
          g orig-ctx new-ctx orig-nodes relevant-port-labels)
  (copy-into g orig-ctx
               new-ctx
               orig-nodes
               relevant-port-labels
               copy-as-placeholder-or-t))

;TODO Move this under node-creation
; A placeholder's ctor is not called. It gets a class and no other attributes
; except 'placeholder? = #t.
(: make-placeholder : Graph Symbol -> (Values Graph Node))
(define (make-placeholder g classname)
  (make-node g (hash 'class classname 'placeholder? #t)))

(: mark-copying : Graph Node Node -> Graph)
(define (mark-copying g from-node to-node)
  (add-edge g `((,from-node copying-to) (,to-node copying-from))))

(: copying-from : Graph Node -> (U Node Void))
(define (copying-from g node)
  (port->neighbor g `(,node copying-from)))

(: copying-to : Graph Node -> (U Node Void))
(define (copying-to g node)
  (port->neighbor g `(,node copying-to)))

;; ======================================================================
;;
;; JSON for D3
;;

;TODO Make a GraphForD3 type?

; Returns g in JSON, in a form suitable for sending to the Javascript code
; that renders the graph by invoking D3.js.
(: graph->d3 : Graph -> JSExpr)
(define (graph->d3 g)
  (let ([ht/node->attrs
          (for/fold : (Hashof Node Attrs)
                    ([ht : (Hashof Node Attrs) (graph->ht/node->attrs g)])
                    ([node (all-nodes g)])
            (add-d3-derived-attrs g ht node))]
;            (let ([(ht _) (d3width g ht node)]
;                  [(ht _) (d3height g ht node)]
;                  [ht (add-attr/members g ht node)]
;                  [ht (add-attr/members-recursive g ht node)]
;                  [ht (add-attr/member-of g ht node)])
;              ht))]
         [t (graph-get-var g 't "?")])
    (hash 'nodes (->jsexpr (hash-values ht/node->attrs))
          'links (edges->jsexpr g)
          't (->jsexpr t))))

(define d3-derived-attrs : (Hashof Symbol (Graph Node -> Any))
  (hash 'members members-of
        'membersRecursive members-of/rec
        'memberOf member-of))

(: add-d3-derived-attrs : Graph (Hashof Node Attrs) Node -> (Hashof Node Attrs))
(define (add-d3-derived-attrs g ht node)
  (let ([(ht _) (d3width g ht node)]
        [(ht _) (d3height g ht node)])
    (for/fold ([ht : (Hashof Node Attrs) ht])
              ([(key fn) (in-hash d3-derived-attrs)])
      (add-attr ht node key (fn g node)))))

;(: add-attr/members : Graph (Hashof Node Attrs) Node -> (Hashof Node Attrs))
;(define (add-attr/members g ht node)
;  (add-attr ht node 'members (members-of g node)))
;
;(: add-attr/members-recursive : Graph (Hashof Node Attrs) Node
;                                -> (Hashof Node Attrs))
;(define (add-attr/members-recursive g ht node)
;  (add-attr ht node 'membersRecursive (set->list (members-of/rec g node))))

(: add-attr : (Hashof Node Attrs) Node Symbol Any -> (Hashof Node Attrs))
(define (add-attr ht node key val)
  (let ([new-attrs (cond
                     #:define attrs (hash-ref ht node)
                     [attrs (hash-set attrs key val)]
                     [else (hash key val)])])
    (hash-set ht node new-attrs)))

(define MIN-D3-WIDTH : Integer 2)
(define MIN-D3-CONTAINER-WIDTH : Integer 3)
(define MIN-D3-HEIGHT : Integer 2)
(define MIN-D3-CONTAINER-HEIGHT : Integer 3)

; Figures out the width that node should get when rendered by D3. Returns
; a new attrs table, updated for the widths of all other nodes calculated
; along the way, and the width of node.
;
; The width of a node is MIN-D3-WIDTH if the node is not a container,
; 1 + the sum of the widths of its members if it is a container, with a
; lower bound of MIN-D3-CONTAINER-WIDTH.
(: d3width : Graph (Hashof Node Attrs) Node [#:in-progress (Setof Node)]
             -> (Values (Hashof Node Attrs) Integer))
(define (d3width g ht/node->attrs node #:in-progress [in-progress empty-set])
  (: return : (Hashof Node Attrs) Integer
              -> (Values (Hashof Node Attrs) Integer))
  (define (return ht/node->attrs w)
    (cond
      [(hash-ref ht/node->attrs node (const #f))
       => (λ ([attrs : Attrs])
            (values (hash-set ht/node->attrs node (hash-set attrs 'd3width w))
                    w))]
      [else (values (hash-set ht/node->attrs node (hash 'd3width w))
                    w)]))
  (define existing-width : (U Integer #f)
    (let ([attrs (hash-ref ht/node->attrs node (const #f))])
      (and attrs (cast (hash-ref attrs 'd3width (const #f)) (U Integer #f)))))
  (cond
    [existing-width
     (values ht/node->attrs existing-width)]
    [(set-member? in-progress node) ; cyclic containment?
     (values ht/node->attrs MIN-D3-WIDTH)]
    [(container? g node)
     (let ([in-progress (set-add in-progress node)]
           [(ht w) (for/fold : (Values (Hashof Node Attrs) Integer)
                             ([ht : (Hashof Node Attrs) ht/node->attrs]
                              [w 1])
                             ([m (members-of g node)])
                     (let ([(ht member-width)
                            (d3width g ht m #:in-progress in-progress)])
                       (values ht (+ w member-width))))])
       (return ht (max w MIN-D3-CONTAINER-WIDTH)))]
    [else (return ht/node->attrs MIN-D3-WIDTH)]))

; Same as d3width but calculates height.
(: d3height : Graph (Hashof Node Attrs) Node [#:in-progress (Setof Node)]
              -> (Values (Hashof Node Attrs) Integer))
(define (d3height g ht/node->attrs node #:in-progress [in-progress empty-set])
  (: return : (Hashof Node Attrs) Integer
              -> (Values (Hashof Node Attrs) Integer))
  (define return (return/ node 'd3height))
  (define existing-height : (U Integer #f)
    (let ([attrs (hash-ref ht/node->attrs node (const #f))])
      (and attrs (cast (hash-ref attrs 'd3height (const #f)) (U Integer #f)))))
  (cond
    [existing-height
     (values ht/node->attrs existing-height)]
    [(set-member? in-progress node) ; cyclic containment?
     (values ht/node->attrs MIN-D3-HEIGHT)]
    [(container? g node)
     (let ([in-progress (set-add in-progress node)]
           [(ht h) (for/fold : (Values (Hashof Node Attrs) Integer)
                             ([ht : (Hashof Node Attrs) ht/node->attrs]
                              [h 1])
                             ([m (members-of g node)])
                     (let ([(ht member-height)
                            (d3height g ht m #:in-progress in-progress)])
                       (values ht (+ h member-height))))])
       (return ht/node->attrs (max h MIN-D3-CONTAINER-HEIGHT)))]
     [else (return ht/node->attrs MIN-D3-HEIGHT)]))

; Helper function for d3width and d3height.
(: return/ : Node Symbol -> (-> (Hashof Node Attrs) Integer
                                (Values (Hashof Node Attrs) Integer)))
(define (return/ node key)
  (λ ([ht : (Hashof Node Attrs)] [v : Integer])
    (cond
      [(hash-ref ht node (const #f))
       => (λ ([attrs : Attrs])
            (values (hash-set ht node (hash-set attrs key v))
                    v))]
      [else (values (hash-set ht node (hash key v))
                    v)])))

