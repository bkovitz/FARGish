; model1.rkt -- Generic run-time code for FARG model
;
; Goes with numbo1.rkt. Probably will be obsolete soon after numbo1.rkt is done.

#lang debug at-exp racket

(require debug/repl errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

;TODO Global constants should be stored as variables in the graph.
(define max-timesteps 10)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)
(define salience-decay 0.8)

(define (make-empty-graph spec)
  (struct-copy g:graph g:empty-graph [spec spec]))

(define (make-choice-pairs f choices)
  (for/fold ([pairs '()] [w 0.0])
            ([choice choices])
        (let ([delta (f choice)])
          (if (zero? delta)
            (values pairs w)
            (values (cons `(,w . ,choice) pairs)
                    (+ delta w))))))

;TODO Move this to another file
(define (weighted-choice-by f choices)
  (define-values (choice-pairs total) (make-choice-pairs f choices))
  (cond
    [(null? choice-pairs)
     (void)]
    [(null? (cdr choice-pairs))
     (cdar choice-pairs)]
    [else (let ([r (* (random) total)])
            (let loop ([choice-pairs choice-pairs])
              (let ([pair (car choice-pairs)])
                (cond
                  [(>= r (car pair))
                   (cdr pair)]
                  [else (loop (cdr choice-pairs))]))))]))

(define (select-choice-pair r choice-pairs)
  (let ([first-item-weight (caar choice-pairs)])
    (cond
      [(<= r first-item-weight)
       (values (car choice-pairs) (cdr choice-pairs))]
      [else (let-values ([(pair rest)
                            (select-choice-pair
                              (- r first-item-weight)
                              (cdr choice-pairs))])
              (values pair (cons (car choice-pairs) rest)))])))

(define (seq-weighted-by f choices)
  (define-values (choice-pairs total) (make-choice-pairs f choices))
  (let loop ([choice-pairs choice-pairs] [total total])
    (cond
      [(null? choice-pairs)
       '()]
      [else (let ([r (* (random) total)])
              (let-values ([(pair choice-pairs)
                              (select-choice-pair r choice-pairs)])
                (cons (cdr pair)
                      (loop choice-pairs (- total (car pair))))))])))

;; ======================================================================
;;
;; Access to a few things without a prefix
;;

(define graph? g:graph?)
(define get-node-attr g:get-node-attr)
(define set-node-attr g:set-node-attr)
(define update-node-attr g:update-node-attr)
(define get-node-attrs g:get-node-attrs)
(define union-node-attrs g:union-node-attrs)
(define node-attr? g:node-attr?)
(define members-of g:members-of)
(define member-of g:member-of)
(define member-of? g:member-of?)
(define all-nodes g:all-nodes)
(define copy-graph g:copy-graph)
(define port->neighboring-ports g:port->neighboring-ports)
(define port->neighboring-port g:port->neighboring-port)
(define port->neighbors g:port->neighbors)
(define port->neighbor g:port->neighbor)
(define port-has-neighbor? g:port-has-neighbor?)
(define port-neighbor? g:port-neighbor?)
(define port->port-label->nodes g:port->port-label->nodes)
(define port->incident-edges g:port->incident-edges)
(define port->incident-hops g:port->incident-hops)
(define has-neighbor-from-port-label? g:has-neighbor-from-port-label?)
(define has-node? g:has-node?)
(define has-edge? g:has-edge?)
(define node->neighbors g:node->neighbors)
(define node->ports g:node->ports)
(define node->incident-hops g:node->incident-hops)
(define nodes->hop-between g:nodes->hop-between)
(define other-node g:other-node)
(define other-port-label g:other-port-label)
(define edge->set g:edge->set)
(define edge->list g:edge->list)
(define edge->nodes g:edge->nodes)
(define port->node g:port->node)
(define port->port-label g:port->port-label)
(define hop->from-node g:hop->from-node)
(define hop->to-node g:hop->to-node)

(define graph-set-var g:graph-set-var)
(define graph-get-var g:graph-get-var)
(define graph-update-var g:graph-update-var)
(define graph-remove-var g:graph-remove-var)
(define graph-push-var g:graph-push-var)
(define graph-push-and-set-var g:graph-push-and-set-var)
(define graph-pop-var g:graph-pop-var)

(define as-member f:as-member)
(define as-tag f:as-tag)
(define by-ports f:by-ports)

;; ======================================================================
;;
;; Functions to access elements of a FARG model specification
;;
;; Where appropriate, these functions' first argument can be either a graph
;; (that contains a spec) or a spec.
;;

(define (get-spec g-or-spec)
  (cond
    [(f:farg-model-spec*? g-or-spec) g-or-spec]
    [(g:graph? g-or-spec) (g:graph-spec g-or-spec)]
    [else (raise-arguments-error 'get-spec
                                 @~a{Can't get spec from @|g-or-spec|.})]))

(define (get-nodeclasses x)
  (cond
    [(f:farg-model-spec*? x) (f:farg-model-spec*-nodeclasses x)]
    [(g:graph? x) (get-nodeclasses (g:graph-spec x))]
    [else (raise-arguments-error 'get-nodeclasses
                                 @~a{Can't get nodeclasses from @|x|.})]))

(define (get-nodeclass* g-or-spec nc-or-classname)
  (cond
    [(f:nodeclass*? nc-or-classname) nc-or-classname]
    [else (f:get-nodeclass* (get-spec g-or-spec) nc-or-classname)]))

(define (nodeclass*-of g node)
  (hash-ref (get-nodeclasses g) (class-of g node)
            (λ () (raise-arguments-error 'nodeclass*-of
                    @~a{Node @node has undefined class: @(class-of g node)}))))

(define (get-links-into g node ctx)
  (define nc (nodeclass*-of g node))
  (define ctx-class (class-of g ctx))
  (define args (get-node-attr g node 'args))
  (define spec (get-spec g))
  (let ([lis (filter (λ (li)
                       (f:nodeclass-is-a? spec
                         ctx-class
                         (f:links-into*-ctx-class li)))
                     (f:get-nodeclass-attr nc 'links-into args))])
    (if (null? lis) (list (f:links-into* ctx (list as-member))) lis)))

(define (max-neighbors-of-port g port)
  (match-define `(,node ,port-label) port)
  (define portclass (f:get-portclass* (get-spec g) port-label))
  (cond
    [(f:portclass*? portclass)
     (f:get-portclass-attr portclass 'max-neighbors +inf.0)]
    [else +inf.0]))

(define (get-nodeclass-attr g node key)
  (define args (get-node-attr g node 'args))
  (f:get-nodeclass-attr (nodeclass*-of g node) key args)) 

;; ======================================================================
;;
;; Functions to make nodes and edges
;;

(define/g (make-node g classname . args)
  (let*-values ([(nodeclass) (get-nodeclass* g classname)]
                [(_) (when (void? nodeclass)
                       (raise-arguments-error 'make-node
                         @~a{No such class: @classname
                             args = @args}))]
                [(attrs) (f:args->node-attrs nodeclass args)]
                [(g node) (g:make-node g attrs)])
    (post-make-node g node)))

(define (post-make-node g node)
  (let ([g (record-new-node g node)]
        [g (boost-salience-of g node)])
    (values g node)))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (make-node/in g ctx classname . args)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (apply make-node g classname args)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (f:links-into*-by-portss links-into)])
      (match-define (f:by-ports* from-port-label to-port-label) by-ports)
      (g:add-edge g `((,ctx ,from-port-label) (,node ,to-port-label))))))

(define/g (add-node/in g . args)
  (first-value (apply make-node/in g args)))

(define/g (add-nodes/in g ctx classname vs)
  (for/fold ([g g])
            ([value vs])
    (add-node/in g ctx classname value)))

; A placeholder's ctor is not called. It gets a class and no other attributes
; except 'placeholder? = #t.
(define (make-placeholder g classname)
  (let ([(g node) (g:make-node g (hash 'class classname 'placeholder? #t))])
    (post-make-node g node)))

(define/g (remove-nodes/in g ctx)
  (for/fold ([g g])
            ([node (members-of g ctx)])
    (g:remove-node g node)))

;edge is '((node1 port-label1) (node2 port-label2)) or a set of those.
;Doesn't add the edge if it already exists, but will change its weight.
;Adds both nodes to the 'touched-nodes set regardless of whether the edge
;already exists.
(define/g (add-edge g edge [weight 1.0])
  (for/fold ([g (g:add-edge g edge weight)])
            ([node (g:edge->nodes edge)])
    (touch-node g node)))

(define/g (touch-node g node)
  (g:graph-update-var g 'touched-nodes
    (λ (touched-so-far) (set-add touched-so-far node))
    empty-set))

(define/g (clear-touched-nodes g)
  (g:graph-set-var g 'touched-nodes empty-set))

(define/g (record-new-node g node)
  (g:graph-update-var g 'new-nodes
    (λ (so-far) (set-add so-far node))
    empty-set))

(define/g (new-nodes g)
  (g:graph-get-var g 'new-nodes empty-set))

(define/g (clear-new-nodes g)
  (g:graph-set-var g 'new-nodes empty-set))

(define (new-node? g node)
  (set-member? (new-nodes g) node))

;; ======================================================================
;;
;; Functions to query nodes
;;

(define (args-of g node)
  (get-node-attr g node 'args))

(define (name-of g node)
  (get-node-attr g node 'name))

(define (ids->names g ids)
  (for/list ([id ids])
    (name-of g id)))

(define (class-of g node)
  (get-node-attr g node 'class))

(define (value-of g node)
  (get-node-attr g node 'value))

(define (value-of-equal? g v node)
  (define node-value (value-of g node))
  (and (not (void? node-value)) (equal? v node-value)))

(define (tag? g node)
  (g:node-attr? g node 'tag?))

(define (non-tag? g node)
  (not (tag? g node)))

(define (node-is-a? g node ancestor)
  (f:nodeclass-is-a? (get-spec g) (nodeclass*-of g node) ancestor))

;; ======================================================================
;;
;; Path
;;

;(: path->node : Graph Node Port-label * -> (U Void Node))
(define (path->node g node . port-labels)
  (let loop ([node node] [port-labels port-labels])
    (cond
      [(null? port-labels) node]
      #:define next-node (port->neighbor g `(,node ,(car port-labels)))
      [(void? next-node) (void)]
      [else (loop next-node (cdr port-labels))])))

;; ======================================================================
;;
;; Functions to search for nodes
;;

(define (find-in g nodeclass-name ctx)
  (let loop ([nodes (members-of g ctx)])
    (cond
      [(null? nodes) (void)]
      [(node-is-a? g (car nodes) nodeclass-name) (car nodes)]
      [else (loop (cdr nodes))])))

;(: walk : Graph Node (-> Graph Node (Listof Node)) -> (Setof Node))
(define (walk g start-node node->next-nodes)
  (let loop ([result (set)]
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
;(: node->neighbors/port-label/ Port-label -> (-> Graph Node -> (Listof ;Node)))
(define (node->neighbors/port-label/ port-label)
  (λ (g node)
    (port->neighbors g `(,node ,port-label))))

; Returns a set of all nodes reachable from start-node's port named port-label,
; and all nodes reachable from those nodes' port named port-label, and so on.
;(: follow-port-label/rec : Graph Node Port-label -> Graph)
(define (follow-port-label/rec g start-node port-label)
  (walk g start-node (node->neighbors/port-label/ port-label)))

;(: members-of/rec : Graph Node -> (Setof Node))
(define (members-of/rec g node)
  (follow-port-label/rec g node 'members))

;(: filter/g : Graph (-> Graph Node Boolean) (Listof Node) -> (Listof Node))
(define (filter/g g pred?/g nodes)
  (for/list ([node nodes]
             #:when (pred?/g g node))
    node))

(module+ test
  (test-case "members-of/rec"
    (define spec
      (farg-model-spec
        (nodeclass container
          (is-a 'ctx))))
    (let ([g (make-empty-graph spec)]
          [(g c1) (make-node g 'container)]
          [(g c2) (make-node/in g c1 'container)]
          [(g c3) (make-node/in g c1 'container)]
          [(_) (check-equal? (members-of/rec g c1)
                             (set c2 c3))]
          [(g c4) (make-node/in g c2 'container)]
          [(_) (check-equal? (members-of/rec g c1)
                             (set c2 c3 c4))]
          [(_) (check-equal? (members-of/rec g c2)
                             (set c4))]
          [(_) (check-equal? (members-of/rec g c3)
                             (set))])
      (void)))
  
  (test-case "filter/g"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (name n)
          (value n))
        ;(tagclass a-tag)  TODO This is all that should be necessary
        (tagclass a-tag
          (applies-to ([node])
            (condition (const #t))))
        ))
    (let ([g (make-empty-graph spec)]
          [(g n1) (make-node g 'number 1)]
          [(g n2) (make-node g 'number 2)]
          [(g n3) (make-node g 'number 3)]
          [(g n4) (make-node g 'number 4)]
          [(g t1) (make-tag g '(a-tag) n1)]  ; TODO Just 'a-tag, not '(a-tag)
          [(g t2) (make-tag g '(a-tag) n2)]
          [(g t4) (make-tag g '(a-tag) n4)]
          [_ (check-equal? (list->set (filter/g g non-tag? (all-nodes g)))
                           (set n1 n2 n3 n4))]
          [_ (check-equal? (list->set (filter/g g tag? (all-nodes g)))
                           (set t1 t2 t4))])
      (void))))

;; ======================================================================
;;
;; Copying nodes
;;

; Makes a placeholder in new-ctx for each of orig-nodes, calls (after-copy g
; orig-node new-node) after making each node, and makes edges among new-nodes
; and new-ctx corresponding to relevant-port-labels among orig-nodes and
; orig-ctx.
;(: copy-into/as-placeholders : Graph Node Node (Listof Node) (Listof Port-label)
;                               (Graph Node Node -> Graph) -> Graph)
;(define (copy-into/as-placeholders
;          g orig-ctx new-ctx orig-nodes relevant-port-labels after-copy)
;  (define (make-placeholders)
;    (for/fold ([g g] [nodemap (hash orig-ctx new-ctx)])
;              ([orig-node orig-nodes])
;      (let ([(g new-node) (make-placeholder g (class-of g orig-node))]
;            [g (after-copy g orig-node new-node)])
;        (values g (hash-set nodemap orig-node new-node)))))
;  (define (make-edges g nodemap)
;    (let ([orig-node? (hash->pred nodemap)]
;          [relevant-port-label? (set->pred (list->set relevant-port-labels))])
;      (for*/fold ([g g])
;                 ([(orig-node new-node) nodemap]
;                  [port-label relevant-port-labels]
;                  [orig-hop (port->incident-hops g `(,orig-node ,port-label))])
;        (cond
;          #:define neighbor-label (other-port-label orig-hop)
;          [(not (relevant-port-label? neighbor-label))
;           g]
;          #:define orig-neighbor (other-node orig-hop)
;          [(not (orig-node? orig-neighbor))
;           g]
;          #:define new-neighbor (hash-ref nodemap orig-neighbor)
;          [else (add-edge g `((,new-node ,port-label)
;                              (,new-neighbor ,neighbor-label)))]))))
;  (let ([(g nodemap) (make-placeholders)]
;        [g (make-edges g nodemap)])
;    g))

(define (copy-into/as
          g orig-ctx new-ctx orig-nodes relevant-port-labels copy-node)
  (define (make-nodes)
    (for/fold ([g g] [nodemap (hash orig-ctx new-ctx)])
              ([orig-node orig-nodes])
      (let ([(g new-node) (copy-node g orig-node)])
        (values g (hash-set nodemap orig-node new-node)))))
  (define (make-edges g nodemap)
    (let ([orig-node? (hash->pred nodemap)]
          [relevant-port-label? (set->pred (list->set relevant-port-labels))])
      (for*/fold ([g g])
                 ([(orig-node new-node) nodemap]
                  [port-label relevant-port-labels]
                  [orig-hop (port->incident-hops g `(,orig-node ,port-label))])
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

(define (copy-as-placeholder-or-t g orig-node)
  (let ([(g new-node) (cond
                        [(node-is-a? g orig-node 't)
                         (apply make-node g 't (args-of g orig-node))]
                        [else (make-placeholder g (class-of g orig-node))])]
        [g (mark-copying g orig-node new-node)])
    (values g new-node)))

(define (copy-into/as-placeholders
          g orig-ctx new-ctx orig-nodes relevant-port-labels)
  (copy-into/as g orig-ctx
                new-ctx
                orig-nodes
                relevant-port-labels
                copy-as-placeholder-or-t))

;(: mark-copying : Graph Node Node -> Graph)
(define (mark-copying g from-node to-node)
  (add-edge g `((,from-node copying-to) (,to-node copying-from))))

(define (copying-from g node)
  (port->neighbor g `(,node copying-from)))

(define (copying-to g node)
  (port->neighbor g `(,node copying-to)))

(module+ test
  (test-case "copy-into/as-placeholders"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (name n)
          (value n))
        (nodeclass c
          (is-a 'ctx))))

    (let ([g (make-empty-graph spec)]
          [(g c1) (make-node g 'c)]
          [(g c2) (make-node g 'c)]
          [g (add-nodes/in g c1 'number '(1 2 3))]
          [g (add-edge g `((1 next) (2 prev)))]
          [g (add-edge g `((2 next) (3 prev)))]
          [g (add-edge g `((1 next) (3 ignore)))]
          [g (copy-into/as-placeholders g
                                        c1 c2
                                        (list 1 2 3)
                                        '(members member-of prev next)
                                        mark-copying)]
          [(n1 n2 n3) (values (copying-to g 1)
                              (copying-to g 2)
                              (copying-to g 3))]
          [member-of-c2? (λ (n) (member-of? g c2 n))]
          [_ (check-pred member-of-c2? n1)]
          [_ (check-pred member-of-c2? n2)]
          [_ (check-pred member-of-c2? n3)]
          [_ (check-true (has-edge? g `((,n1 next) (,n2 prev))))]
          [_ (check-true (has-edge? g `((,n2 next) (,n3 prev))))]
          [_ (check-true (not (has-edge? g `((,n1 next) (,n3 ignore)))))])
      (void))))

;; ======================================================================
;;
;; Miscellaneous node functions
;;

;(: make-member-of : Graph Node Node * -> Graph)
(define (make-member-of g ctx . nodes)
  (for/fold ([g g])
            ([node nodes])
    (add-edge g `((,ctx members) (,node member-of)))))

;; ======================================================================
;;
;; Predicates
;;

;TODO Deal correctly with arity of pred?. This version assumes arity 2.
;(define/g (value-pred? g pred? . nodes)
;  (values g
;    (cond
;      [(null? nodes) #t]
;      [(null? (cdr nodes)) #t]  ; TODO Is this right?
;      [else (let* ([node1 (car nodes)] [v (value-of g node1)])
;              (for/and ([node (cdr nodes)])
;                (pred? v (value-of g node))))])))

(define/g (value-pred? g pred? . nodes)
  (values g
    (let ([v-of (λ (node) (value-of g node))])
      (apply pred? (map v-of nodes)))))

(define/g (and? g . preds/g)
  (values g
    (for/and ([pred?/g preds/g])
      (apply/ignore-g pred?/g g))))

(define/g (same-class? g . nodes)
  (values g
    (cond
      [(null? nodes) #f]
      [(null? (cdr nodes)) #t]
      [else
        (let ([class-of-node1 (class-of g (car nodes))])
          (for/and ([other-node (cdr nodes)])
            (equal? class-of-node1 (class-of g other-node))))])))

;; ======================================================================
;;
;; Salience
;;

(define (node? x)
  (or (symbol? x) (integer? x)))

;(: salience-of : Graph Node/Edge -> Flonum)
(define (salience-of g elem)
  (cond
    [(node? elem)
     (let ([s (get-node-attr g elem 'salience)])
       (if (void? s) 0.0 s))]
    [else
      (let ([sal (λ (node) (salience-of g node))])
        (apply max (map sal (edge->nodes elem))))]))

(define (boost-salience-of g node)
  (g:update-node-attr g node 'salience
    (curry + 1.0)
    0.0))

(define (boost-salience-of-touched-nodes g)
  (for/fold ([g g])
            ([node (g:graph-get-var g 'touched-nodes)])
    (boost-salience-of g node)))

(define (reduce-salience-of g node)
  (g:update-node-attr g node 'salience
    (curry * 0.5)
    0.0))

(define (decay-saliences-in g ctx)
  (for/fold ([g g])
            ([node (members-of g ctx)])
    (decay-salience-of g node)))

(define (decay-salience-of g node)
  (g:update-node-attr g node 'salience
      (curry * salience-decay)
      0.0))

;(: decay-salience : Graph -> Graph)
(define (decay-salience/all g)
  (for/fold ([g g])
            ([node (all-nodes g)])
    (decay-salience-of g node)))

(define (seq-weighted-by-salience g nodes)
  (seq-weighted-by (λ (node) (salience-of g node))
                   nodes))

(define (g->node->neighbors g)
  (λ (node) (node->neighbors g node)))

; -> (Setof Node)
(define (nearby-nodes g node #:num-hops [num-hops 2] #:filter [fi #f])
  (if (zero? num-hops)
    empty-set
    (let* ([node->neighbors (g->node->neighbors g)]
           [nodes->neighbors (λ (st)
                               (for/fold ([result empty-set])
                                         ([node (in-set st)])
                                 (set-union result (node->neighbors node))))]
           [accumulate (if fi
                         (λ (result nodes)
                           (for/fold ([result result])
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

(define (g->node->salience g)
  (λ (node) (salience-of g node)))

; Returns Node or #f if no node found.
(define (choose-nearby-node-by-salience g node #:num-hops [num-hops 2]
                                               #:filter [fi (const #t)])
  (let ([nodes (set->list (nearby-nodes g node #:num-hops num-hops
                                               #:filter fi))])
    (cond
      [(null? nodes) #f]
      [else (weighted-choice-by (g->node->salience g) nodes)])))

(define (display-salience g node [salience (void)])
  (let ([salience (if (void? salience) (salience-of g node) salience)]
        [s-node (~a (~a node #:min-width 15))]
        [s-salience (~r salience #:precision '(= 3))])
    (displayln (string-append s-node " " s-salience))))

(define (pr-saliences g)
  (for ([node (members-of g 'ws)])
    (display-salience g node)))

(define (saliences-ht g)
  (for/hash ([node (members-of g 'ws)])
    (values node (salience-of g node))))

;; ======================================================================
;;
;; Tagging
;;

; Returns a nodeclass* where all the class-attrs have the args filled in
; as provided in nodespec. nodespec can be a symbol, in which case there
; are no args, or a list where the nodeclass's name is the first element
; and the remaining elements are the args, e.g. '(fills-port result 5).
;TODO Call parse-nodespec
(define (realize-nodespec g nodespec)
  (match nodespec
    [`(,class-name . ,args)
     (list class-name args)
     (f:realize-attrs (get-nodeclass* g class-name) args)]
    [(? symbol?)
     (f:realize-attrs (get-nodeclass* g nodespec) '())]
    [else (raise-arguments-error 'realize-nodespec
                                 @~a{Invalid nodespec: @|nodespec|.})]))

; Returns two values: class-name, args
(define (parse-nodespec nodespec)
  (match nodespec
    [`(,class-name . ,args)
     (values class-name args)]
    [(? symbol?)
     (values nodespec '())]
    [else (raise-arguments-error 'realize-nodespec
                                 @~a{Invalid nodespec: @|nodespec|.})]))

; taggee-info* ti must be fully realized (no args needed).
(define (taggee-info-match? g ti node)
  (let ([spec (get-spec g)]
        [nclass (class-of g node)])
    (define of-classes (f:taggee-info*-of-classes ti))
    (if (null? of-classes)
      #t
      (for/or ([ofc of-classes])
        (f:nodeclass-is-a? spec nclass ofc)))))

; Calls f. If f returns a single value, we return it. If f returns multiple
; values, as many graph functions do, we return only the second value, ignoring
; the first. (The first value, by convention, is an updated g.) This is
; something of a HACK, since a caller should propagate an updated g, not
; throw it away, but it's getting to be a pain to remember which functions
; return an updated g and which don't. There are surely better ways to handle
; this, like redefining #%app to handle gfuncs correctly, or static
; type-checking.
(define (apply/ignore-g f g . args)
  (call-with-values (λ () (apply f g args))
    (case-lambda
      [(result) result]
      [(g result . ignored) result])))

; The condition function c must be fully realized (no args needed).
(define (condition-match? g c . nodes)
  (define cfunc-takes-g (apply c nodes))
  (call-with-values (λ () (cfunc-takes-g g))
    (case-lambda 
      [(result) result]
      [(g result . ignored) result])))

(define (tagclass-applies-to? g tagspec . nodes)
  (if (null? nodes)
    #f
    (let ([tagclass (realize-nodespec g tagspec)])
      (for/or ([applies-to (f:get-raw-nodeclass-attr tagclass 'applies-to)])
        (and (for/and ([ti (f:applies-to*-taggee-infos applies-to)]
                       [node nodes])
               (taggee-info-match? g ti node))
             (for/or ([c (f:applies-to*-conditions applies-to)])
               (apply condition-match? g c nodes)))))))

(define (tagspec->port-labels g tagspec)
  (define tagclass (realize-nodespec g tagspec))
  (for*/set ([applies-to (f:get-raw-nodeclass-attr tagclass 'applies-to)]
             [ti (f:applies-to*-taggee-infos applies-to)]
             [by-ports (f:taggee-info*-by-portss ti)])
    (f:by-ports*-from-port-label by-ports)))

;Something of a HACK. This simply takes the neighbors at the union of the
;tag class's taggee-infos' by-portss. Properly, it should look only at
;port labels in one applies-to* that actually applies to the tag. Perhaps
;the best way to resolve this is just to define a rule about what counts
;as a taggee, which is fast to compute. Maybe require that tag and taggee
;ports be defined as portclasses that inherit from 'tag-port and 'taggee-port.
;Returns a set.
(define (taggees-of g tag)
  (define port-labels (tagspec->port-labels g (tagspec-of g tag)))
  (apply set-union empty-set (for/list ([port-label port-labels])
                               (list->set
                                 (g:port->neighbors g `(,tag ,port-label))))))


;HACK This is way too model- and tag-specific.
(define (tags-of g node)
  (g:port->neighbors g `(,node tags)))

;TODO UT
(define (tags-of-class g class node)
  (for/list ([tag (tags-of g node)]
             #:when (node-is-a? g tag class))
    tag))

;HACK This is way too model- and tag-specific.
(define (value-of-taggee g tag)
  (value-of g (g:port->neighbor g `(,tag tagged))))

(define (tagspec-of g tag)
  (define tagclass (class-of g tag))
  (define args (args-of g tag))
  (cond
    [(void? args) tagclass]
    [(null? args) tagclass]
    [else (cons tagclass args)]))

;TODO UT
(define (tag-still-applies? g tag)
  (apply tagclass-applies-to? g (tagspec-of g tag) (taggees-of g tag)))

;TODO BUG This seems to ignore the args.
;TODO OAOO: There's some redundancy between this function and
;tagclass-applies-to?
(define (tagged-with? g tagspec . nodes)
  (if (null? nodes)
    #f
    (let ([tagclass (realize-nodespec g tagspec)])
      (for/or ([applies-to (f:get-raw-nodeclass-attr tagclass 'applies-to)])
        (linked-from-common-node? g
          (f:applies-to*-taggee-infos applies-to) nodes)))))

;HACK
(define (has-tag? g tagspec node)
  (for/or ([neighbor (g:port->neighbors g `(,node tags))])
    (matches-tagspec? g tagspec neighbor)))

(define (matches-tagspec? g tagspec node)
  (let-values ([(spec-class-name spec-args) (parse-nodespec tagspec)])
    (and (node-is-a? g node spec-class-name)
         (equal? spec-args (get-node-attr g node 'args)))))

;TODO Handle case where taggee-infos is empty
(define (linked-from-common-node? g taggee-infos nodes)
  (let/cc break
    (for/fold ([back-nodes (void)] #:result (not (set-empty? back-nodes)))
              ([taggee-info taggee-infos] [node nodes])
      (let ([back-nodes (set-intersect* back-nodes
                                        (linked-from g taggee-info node))])
        (if (set-empty? back-nodes)
          (break #f)
          back-nodes)))))

(define (linked-from g taggee-info node)
  (let/cc break
    (for/fold ([froms (void)] #:result (if (void? froms) empty-set froms))
              ([by-ports (in-list (f:taggee-info*-by-portss taggee-info))])
      (match-define (f:by-ports* from-port-label to-port-label) by-ports)
      (let ([froms (set-intersect* froms
                                   (g:port->port-label->nodes g
                                     `(,node ,to-port-label)
                                     from-port-label))])
        (if (set-empty? froms)
          (break froms)
          froms)))))

(define (common-ctxs g nodes)
  (apply set-intersect (for/list ([node nodes])
                         (list->set (g:member-of g node)))))

(define (link-as-member g ctxs node)
  (for/fold ([g g])
            ([ctx ctxs])
    (g:add-edge g `((,ctx members) (,node member-of)))))

(struct Fizzle (tagclass nodes) #:prefab)

;TODO Make the tag a member of the nodes' least common ctx
;TODO Don't make the tag if it's already there
(define/g (make-tag g tagspec . nodes)
  (let*-values ([(tagclass) (realize-nodespec g tagspec)]
                [(tagclass-name args) (parse-nodespec tagspec)])
    (cond
      [(first-matching-applies-to g tagclass nodes)
       => (λ (applies-to)
            (let*-values ([(g tag) (apply make-node g tagclass-name args)])
              (for/fold ([g g]
                         #:result (values
                                    (link-as-member g (common-ctxs g nodes) tag)
                                    tag))
                        ([taggee-info (f:applies-to*-taggee-infos applies-to)]
                         [node nodes])
                (link-to g taggee-info tag node))))]
      [else (raise (Fizzle tagspec nodes))])))

(define/g (add-tag g tagspec . nodes)
  (if (for/and ([node nodes])
        (has-tag? g tagspec node)) ;HACK!!!!
    g
    (apply add-tag-even-if-already-there g tagspec nodes)))

(define/g (add-tag-even-if-already-there g tagspec . nodes)
  (first-value (apply make-tag g tagspec nodes)))

(define/g (add-tags g tagspecs . nodes)
  (for/fold ([g g])
            ([tagspec tagspecs])
    (apply add-tag g tagspec nodes)))

(define/g (link-to g by-portss from-node to-node)
  (let* ([by-portss (match by-portss
                      [(struct* f:taggee-info* ([by-portss bps])) bps]
                      [(struct* f:links-into* ([by-portss bps])) bps]
                      [else
                        (raise-arguments-error 'link-to
                          @~a{Can't extract by-portss from @|by-portss|.})])])
    (for/fold ([g g])
              ([by-ports by-portss])
      (match-define (f:by-ports* from-port-label to-port-label) by-ports)
      (g:add-edge g `((,from-node ,from-port-label) (,to-node ,to-port-label))))))

(define (first-matching-applies-to g realized-tagclass nodes)
  ;(define nodeclass (get-nodeclass* g realized-tagclass))
  (define applies-tos (f:get-raw-nodeclass-attr realized-tagclass 'applies-to))
  (for/or ([applies-to applies-tos])
    (applies-to? g applies-to nodes)))

(define (condition-func-passes? g cfunc nodes)
  (define cfunc-takes-g (apply cfunc nodes))
  (cfunc-takes-g g))

(define (possible-taggee? g taggee-info node)
  (define of-classes (f:taggee-info*-of-classes taggee-info))
  (if (null? of-classes)
    #t
    (for/or ([of-class of-classes])
      (node-is-a? g node of-class))))

(define (all-taggee-infos-could-apply? g applies-to nodes)
  (define taggee-infos (f:applies-to*-taggee-infos applies-to))
  (if (not (= (length taggee-infos) (length nodes)))
    #f
    (for/and ([taggee-info taggee-infos]
              [node nodes])
      (possible-taggee? g taggee-info node))))

;; Returns #f or the applies-to*.
(define (applies-to? g applies-to nodes)
  (and (all-taggee-infos-could-apply? g applies-to nodes)
       ;TODO OAOO with tagclass-applies-to?
       (for/or ([c (f:applies-to*-conditions applies-to)])
         (apply condition-match? g c nodes))
       applies-to))

;; ======================================================================
;;
;; Predefined vars
;;

(define (bump-t g)
  (graph-set-var g 't (add1 (graph-get-var g 't 0))))

;; ======================================================================
;;
;; Printing
;;

(define (pr-nodeclass-of g nodeid)
  (pretty-print (f:realize-attrs (nodeclass*-of g nodeid)
                                 (args-of g nodeid))))

; TODO Remove; replacing calling code with calls to log/a in logging.rkt.
(define (log . args)
  (define as (string-join (for/list ([arg args])
                            (~a arg))))
  (displayln @~a{  @as}))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "spec basics"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (value n)
          (name n))
        (nodeclass (brick n)
          (is-a 'number)
          (links-into 'ctx (by-ports 'bricks 'source) as-member))))

    (define g (make-empty-graph spec))

    (define ws (gdo make-node 'ws))
    (define number22 (gdo make-node/in 'ws 'number 22))
    (define brick7 (gdo make-node/in ws 'brick 7))

    (check-equal? (list->set (g:all-nodes g))
                  (set ws number22 brick7))

    ; display-names
    (check-equal? (get-node-attr g number22 'name) 22)
    (check-equal? (get-node-attr g brick7 'name) 7)

    ; values
    (check-equal? (get-node-attr g number22 'value) 22)
    (check-equal? (get-node-attr g brick7 'value) 7)

    ; linking into a ctx
    (check-true (g:port-neighbor? g `(,ws bricks) brick7))
    (check-equal? (list->set (g:port->neighboring-ports g `(,ws members)))
                  (set `(,number22 member-of) `(,brick7 member-of)))

    ; is-a ancestor relations
    (check-true (node-is-a? g brick7 'brick))
    (check-true (node-is-a? g brick7 'number))
    (check-true (node-is-a? g number22 'number))
    (check-false (node-is-a? g number22 'brick)))
  
  (test-case "tagging"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (value n)
          (name n))
        (nodeclass (brick n)
          (is-a 'number)
          (links-into 'ctx (by-ports 'bricks 'source) as-member))
        (nodeclass (neither n)
          (value n))
        (tagclass (same nc)  ; same value, both is-a nc
          (applies-to ([node1 (of-class nc) (by-ports 'tagged 'tags)]
                       [node2 (of-class nc) (by-ports 'tagged 'tags)])
            (condition (value-pred?/g = node1 node2))))))

    (define g (make-empty-graph spec))

    (define ws (gdo make-node 'ws))
    (define number22 (gdo make-node/in 'ws 'number 22))
    (define brick7 (gdo make-node/in ws 'brick 7))
    (define brick22 (gdo make-node/in ws 'brick 22))
    (define neither22 (gdo make-node/in ws 'neither 22))

    ;(pr-graph g)

    (check-false (tagclass-applies-to? g '(same number) brick7 number22))
    (check-true (tagclass-applies-to? g '(same number) brick22 number22))
    (check-false (tagclass-applies-to? g '(same number) neither22 number22))
    
    (define tag (gdo make-tag '(same number) brick22 number22))
    (check-true (tagged-with? g '(same number) brick22 number22))
    (check-true (tagged-with? g '(same number) number22 brick22))

    (check-equal? (taggees-of g tag) (set brick22 number22))
    (check-equal? (tags-of g brick22) (list tag))
    (check-equal? (tags-of g number22) (list tag))
    ))


;(define spec
;  (farg-model-spec
;    (nodeclass (number n)
;      (value n)
;      (name n))
;    (nodeclass (brick n)
;      (is-a 'number)
;      (links-into 'ctx (by-ports 'bricks 'source) as-member))
;    (tagclass (same nc)  ; same value, both is-a nc
;      (applies-to ([node1 (of-class nc) (by-ports 'tagged 'tags)]
;                   [node2 (of-class nc) (by-ports 'tagged 'tags)])
;        (condition (value-pred?/g = node1 node2))))
;    ))

;(define g (void))
;(set! g (make-empty-graph spec))
;
;(define ws (gdo make-node 'ws))
;(define number22 (gdo make-node/in 'ws 'number 22))
;(define brick7 (gdo make-node/in ws 'brick 7))
;(define brick22 (gdo make-node/in ws 'brick 22))

;;(tagclass-applies-to? g '(same number) brick7 number22)
;;(tagclass-applies-to? g '(same number) brick22 number22)
