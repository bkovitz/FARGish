; graph1.rkt -- Data structure for port graphs

#lang debug at-exp errortrace racket

(require "wheel.rkt" "id-set.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require racket/hash)
(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe debug/repl racket/enter)

(provide (struct-out graph)
         empty-graph
         ;add-spec
         ;copy-graph-into-graph
         
         make-node
         add-node
         add-edge
         remove-node
         remove-edge
         
         has-node?
         has-edge?
         all-nodes
         all-edges
         ;find-nodes-of-class

         get-node-attr
         set-node-attr
         update-node-attr
         get-node-attrs
         union-node-attrs
         node-attr?

         graph-edge-weight
         set-edge-weight

         port->neighboring-ports
         port->neighbors
         port->neighbor
         port-has-neighbor?
         port-neighbor?
         port->port-label->nodes 
         port->incident-edges
         port->incident-hops
         has-neighbor-from-port-label?

         node->neighbors
         node->ports
         node->incident-hops
         other-node

         edge->set
         edge->list
         edge->nodes
         port->node
         port->port-label
         hop->from-node
         hop->to-node

         pr-node
         pr-graph
         pr-group

         ;class-of
         ;value-of
         ;value-of-equal?
         
         members-of
         member-of
         member-of?
         no-neighbor-at-port? no-neighbor-at-port?/g
         has-neighbor-at-port? has-neighbor-at-port?/g
         ;nodes-of-class-in
         ;bound-from-ctx-to-ctx?

         ;next-to?
         ;bound-to?
         ;bound-from?
         ;succ?
         ;placeholder
         ;placeholder?
         ;group?

         graph-set-var
         graph-get-var
         graph-update-var
         graph-remove-var
         graph-push-var
         graph-push-and-set-var
         graph-pop-var

         copy-graph-into-graph
         copy-graph

         define/g
         gdo
         )

;; ======================================================================
;;
;; The graph representation
;;

(struct graph (ht-node->attrs
               ht-port->neighboring-ports
               edges
               id-set
               stacks  ; hash-table of temp vars for do-graph-edits
               vars    ; hash-table of vars: name -> value
               spec) #:prefab)

(define empty-spec #hash())

(define (make-empty-graph [spec empty-spec])
  (graph #hash() #hash() #hash() empty-id-set #hash() #hash() spec))

(define empty-graph (make-empty-graph))

;; ======================================================================
;;
;; define/g and gdo
;;

; A function whose first argument is a graph, and that returns one or more
; values, the first of which is the updated graph.
(struct gfunc* (f) #:property prop:procedure 0)

(define-syntax-rule (gλ args body0 body ...)
  (gfunc* (λ args body0 body ...)))

(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ... . optargs) body0 body ...)
     (with-syntax* ([name (syntax-property #'name 'gfunc? #t #t)]
                    [name/g (format-id #'name "~a/g" #'name
                                       #:source #'name #:props #'name)])
       #`(begin
           (define name (gλ (g args ... . optargs) body0 body ...))
           (define (name/g args ... . optargs)
             (gλ (g) #,(if (null? (syntax->datum #'optargs))
                         #'(name g args ...)
                         #'(apply name g args ... optargs))))))]))

; A little hack to make it easier to work on graphs in the REPL.
; (gdo makenode 'ws) operates on a variable in the local context called g,
; set!s g to the new graph, and returns the nodeid of the created node.
(define-syntax (gdo stx)
  (syntax-case stx ()
    [(gdo gfunc args ...)
     (with-syntax ([g (format-id #'gdo "g"
                                 #:source #'gdo #:props #'f)])
       #'(call-with-values (λ () (gfunc g args ...))
           (λ (new-g . results)
             (set! g new-g)
             (cond
               [(null? results) (void)]
               [(null? (cdr results)) (car results)]
               [else results]))))]))

(module+ test
  (test-case "define/g"
    (define mk (make-node/g #hash((name . plus))))
    (define g1 (add-node empty-graph #hash((name . plus))))
    (define-values (g2 plusid) (mk empty-graph))
    (check-equal? (all-nodes g2) '(plus))
    (check-equal? (all-nodes g1) (all-nodes g2)))

  (test-case "gdo"
    (define g (make-empty-graph))
    (gdo make-node #hash((name . this-node)))
    (check-equal? (all-nodes g) '(this-node))))

;; ======================================================================
;;
;; Making and removing nodes
;;

; Returns two values: g nodeid
(define/g (make-node g attrs)
  (let*-values ([(attrs name) (ensure-node-name g attrs)]
                [(id-set id) (gen-id (graph-id-set g) name)]
                [(attrs) (hash-set attrs 'id id)]
                [(g) (let ([ht (graph-ht-node->attrs g)])
                       (struct-copy graph g
                         [ht-node->attrs (hash-set ht id attrs)]
                         [id-set id-set]))])
    (values g id)))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (remove-node g node)
  (let ([g (for/fold ([g g])
                     ([edge (node->incident-hops g node)])
             (remove-edge g edge))])
    (struct-copy graph g
      [ht-node->attrs (hash-remove (graph-ht-node->attrs g) node)])))

(define (ensure-node-name g attrs)
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

;; ======================================================================
;;
;; Making and removing edges
;;

(define (edge->set edge)
  (cond
    [(set? edge) edge]
    [(pair? edge) (list->set edge)]
    [else (raise-argument-error 'edge->set
            "edge must be set of two ports or list of two ports" edge)]))

(define (edge->list edge)
  (cond
    [(pair? edge) edge]
    [(set? edge) (set->list edge)]
    [else (raise-argument-error 'edge->list
            "edge must be set of two ports or list of two ports" edge)]))

;edge is '((node1 port-label1) (node2 port-label2)) or a set of those.
(define (edge->nodes edge)
  (for/list ([port edge])
    (port->node port)))

(define port->node car)
(define port->port-label cadr)

;(define (hop->to-node hop)
;  (match-define `(,from-port (,to-node ,_)) hop)
;  to-node)
;
;(define (hop->from-node hop)
;  (match-define `((,from-node ,_) ,to-port) hop)
;  from-node)
(define hop->to-node caadr)
(define hop->from-node caar)

;edge is '((node1 port-label1) (node2 port-label2)) or a set of those.
;Doesn't add the edge if it already exists, but will change its weight.
(define/g (add-edge g edge [weight 1.0])
  (let ([edge (edge->list edge)])
    (match-define `(,port1 ,port2) edge)
    (define edges (graph-edges g))
    (let* ([p->nps (graph-ht-port->neighboring-ports g)]
           [p->nps (hash-update p->nps
                                port1
                                (λ (st) (set-add st port2))
                                empty-set)]
           [p->nps (hash-update p->nps
                                port2
                                (λ (st) (set-add st port1))
                                empty-set)])
      (struct-copy graph g
        [edges (hash-set edges (set port1 port2) weight)]
        [ht-port->neighboring-ports p->nps]))))

;edge is '((node1 port-label1) (node2 port-label2)) or a set of those.
(define/g (remove-edge g e)
  (define edge (edge->list e))
  (match-define `(,port1 ,port2) edge)
  (define edge* (edge->set e))
  (let* ([p->nps (graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps
                              port1
                              (λ (st) (set-remove st port2))
                              empty-set)]
         [p->nps (hash-update p->nps
                              port2
                              (λ (st) (set-remove st port1))
                              empty-set)])
    (struct-copy graph g
      [edges (hash-remove (graph-edges g) edge*)]
      [ht-port->neighboring-ports p->nps])))

;; ======================================================================
;;
;; Querying existence of nodes and edges
;;

(define (has-node? g id)
  (hash-has-key? (graph-ht-node->attrs g) id))

(define (has-edge? g edge)
  (hash-has-key? (graph-edges g) (edge->set edge)))

(define (all-nodes g)
  (hash-keys (graph-ht-node->attrs g)))

(define (all-edges g)
  (hash-keys (graph-edges g)))

(define (graph-edge-weight g edge [weight-if-no-edge (void)])
  ;TODO Why not just call edge->set ?
  (let ([edge (if (set? edge) (set->list edge) edge)])
    (match-define `(,port1 ,port2) edge)
    (define edge* (set port1 port2))
    (hash-ref (graph-edges g) edge* weight-if-no-edge)))

;TODO UT
(define (set-edge-weight g edge new-weight)
  (let ([edge (if (set? edge) (set->list edge) edge)])
    (match-define `(,port1 ,port2) edge)
    (if (has-edge? g edge)
      (struct-copy graph g
        [edges (hash-set (graph-edges g) (set port1 port2) new-weight)])
      g)))

;; ======================================================================
;;
;; Neighbors
;;

(define (port->neighboring-ports g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port '()))

(define (port->neighbors g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (for/list ([neighboring-port (in-set (hash-ref p->nps port empty-set))])
    (match-define (list neighbor _) neighboring-port)
    neighbor))

(define (port->neighbor g port)
  (match (port->neighbors g port)
    ['() (void)]
    [`(,neighbor . ,_) neighbor]))

(define (port-neighbor? g port node)
  (for/or ([neighbor (port->neighbors g port)])
    (equal? neighbor node)))

(define (port-has-neighbor? g port)
  (not (null? (port->neighboring-ports g port))))

;Returns a set of nodes
(define (port->port-label->nodes g from-port to-port-label)
  (for/fold ([nodes empty-set])
            ([nport (port->neighboring-ports g from-port)])
    (match-define `(,nport-node ,nport-label) nport)
    (if (equal? to-port-label nport-label)
      (set-add nodes nport-node)
      nodes)))

;Returns a list of edges, each edge represented as a set
(define (port->incident-edges g port)
  (for/list ([nport (port->neighboring-ports g port)])
    (set port nport)))

;TODO UT
;Returns a list of edges, each edge represented as a list, with port first
(define (port->incident-hops g port)
  (for/list ([nport (port->neighboring-ports g port)])
    (list port nport)))

;TODO Inefficient
(define (node->ports g node)
  (for/list ([port (hash-keys (graph-ht-port->neighboring-ports g))]
             #:when (equal? node (car port)))
    port))

;Returns list of lists, each of which represents an edge; first port of
;edge is the node's port
(define (node->incident-hops g node)
  (for*/list ([port (node->ports g node)]
              [nport (port->neighboring-ports g port)])
    `(,port ,nport)))

;TODO UT
(define (other-node hop)
  (match hop
    [`((,_ ,_) (,node ,_))
      node]))

;Returns set of nodes
(define (node->neighbors g node)
  (for*/set ([port (node->ports g node)]
             [neighbor (port->neighbors g port)])
    neighbor))

;Rewrite this when port labels have inheritance and arguments
(define (port-label-is-a? g port-label ancestor)
  (equal? port-label ancestor))

;Does node have a neighbor nnode that links to node from `(,nnode ,port-label)?
(define (has-neighbor-from-port-label? g node nport-label)
  (for/or ([hop (node->incident-hops g node)])
    (match-define `(,ignored (,nnode ,port-label)) hop)
    (port-label-is-a? g port-label nport-label)))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "make-node"
    (let*-values ([(g) empty-graph]
                  [(_) (check-false (has-node? g 5))]
                  [(_) (check-equal? (all-nodes g) '())]
                  [(g id1) (make-node g #hash((value . 5)))]
                  [(_) (check-equal? id1 5)]
                  [(_) (check-true (has-node? g 5))]
                  [(_) (check-equal? (all-nodes g) (list id1))]
                  [(g id2) (make-node g #hash((value . 5)))]
                  [(_) (check-not-equal? id1 id2)]
                  [(_) (check-true (has-node? g id2))]
                  [(_) (check-equal? (list->set (all-nodes g)) (set id1 id2))]
                  [(g) (remove-node g id1)]
                  [(_) (check-false (has-node? g id1))]
                  [(_) (check-true (has-node? g id2))]
                  [(_) (check-equal? (all-nodes g) (list id2))])
      (void)))

  (test-case "add-edge and remove-edge"
    (let* ([g (add-node empty-graph #hash((class . number) (name . source9)))]
           [g (add-node g #hash((class . plus)))]
           [g (add-edge g '((source9 output) (plus operand)))])
      (check-true (has-edge? g '((source9 output) (plus operand))))
      (check-true (has-edge? g '((plus operand) (source9 output))))
      (check-equal? (graph-edge-weight g '((plus operand) (source9 output)))
                    1.0)
      (let* ([g (remove-edge g '((plus operand) (source9 output)))])
        (check-false (has-edge? g '((source9 output) (plus operand)))))))

  (let* ([ab-graph (add-node empty-graph #hash((class . letter) (name . a)))]
         [ab-graph (add-node ab-graph #hash((class . letter) (name . b)))])
    (test-case "add-edge with weight"
      (let ([g (add-edge ab-graph '((a out) (b in)) 0.62)])
        (check-equal? (graph-edge-weight g '((a out) (b in)))
                      0.62)))
    (test-case "add-edge twice"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.51)]
             [g (add-edge g '((a out) (b in)))])
        (check-equal? (list (set '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (graph-edge-weight g `((a out) (b in)))
                      1.00)))
    (test-case "add-edge twice with weight"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.75)]
             [g (add-edge g '((a out) (b in)) 0.22)])
        (check-equal? (list (set '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (graph-edge-weight g `((a out) (b in)))
                      0.22))))

  (test-case "port-> various neighbor functions"
    (let*-values ([(g node1) (make-node empty-graph #hash((name . node1)))]
                  [(g node2) (make-node g #hash((name . node2)))]
                  [(g node3) (make-node g #hash((name . node3)))]
                  [(g) (add-edge g `((,node1 from-port) (,node2 to-port)))]
                  [(g) (add-edge g `((,node1 from-port) (,node3 to-port)))])
      ; port->neighboring-ports
      (check-equal? (list->set (port->neighboring-ports g `(,node1 from-port)))
                    (set `(,node2 to-port) `(,node3 to-port)))
      (check-equal? (list->set (port->neighboring-ports g `(,node2 to-port)))
                    (set `(,node1 from-port)))
      (check-equal? (list->set (port->neighboring-ports g `(,node3 to-port)))
                    (set `(,node1 from-port)))

      ; port->neighbors
      (check-equal? (list->set (port->neighbors g `(,node1 from-port)))
                    (set node2 node3))
      (check-equal? (port->neighbors g `(,node2 to-port))
                    (list node1))
      (check-equal? (port->neighbors g `(,node3 to-port))
                    (list node1))

      ; port->neighbor
      (check-not-false (let ([neighbor (port->neighbor g `(,node1 from-port))])
                         (or (equal? node2 neighbor)
                             (equal? node3 neighbor))))
      (check-equal? (port->neighbor g `(,node2 to-port)) node1)
      (check-equal? (port->neighbor g `(,node3 to-port)) node1)

      ; port-neighbor?
      (check-not-false (port-neighbor? g `(,node1 from-port) node2))
      (check-not-false (port-neighbor? g `(,node1 from-port) node3))
      (check-not-false (port-neighbor? g `(,node2 to-port) node1))
      (check-not-false (port-neighbor? g `(,node3 to-port) node1))
      (check-false (port-neighbor? g `(,node2 to-port) node3))

      ; port->incident-edges
      (check-equal? (list->set (port->incident-edges g `(,node1 from-port)))
                    (set (set `(,node1 from-port) `(,node2 to-port))
                         (set `(,node1 from-port) `(,node3 to-port))))))

  (test-case "port->port-label->nodes"
    (let*-values ([(g node1) (make-node empty-graph #hash((name . node1)))]
                  [(g tag1) (make-node g #hash((name . tag1)))]
                  [(g tag2) (make-node g #hash((name . tag2)))]
                  [(g tag3) (make-node g #hash((name . tag3)))]
                  [(g) (add-edge g `((,node1 tags) (,tag1 taggee-type-a)))]
                  [(g) (add-edge g `((,node1 tags) (,tag2 taggee-type-a)))]
                  [(g) (add-edge g `((,node1 tags) (,tag3 taggee-type-b)))])
      (check-equal? (port->port-label->nodes g `(,node1 tags) 'taggee-type-a)
                    (set tag1 tag2))
      (check-equal? (port->port-label->nodes g `(,node1 tags) 'taggee-type-b)
                    (set tag3))))

  (test-case "node-> various neighbor functions"
    (let*-values ([(g node1) (make-node empty-graph #hash((name . node1)))]
                  [(g node2) (make-node g #hash((name . node2)))]
                  [(g tag1) (make-node g #hash((name . tag1)))]
                  [(g) (add-edge g `((,node1 right) (,node2 left)))]
                  [(g) (add-edge g `((,node1 tags) (,tag1 taggees)))])
      ; node->ports
      (check-equal? (list->set (node->ports g node1))
                    (set `(,node1 right) `(,node1 tags)))
      (check-equal? (node->ports g node2) (list `(,node2 left)))
      (check-equal? (node->ports g tag1) (list `(,tag1 taggees)))

      ; node->incident-hops
      (check-equal? (list->set (node->incident-hops g node1))
                    (set `((,node1 right) (,node2 left))
                         `((,node1 tags) (,tag1 taggees))))

      ; node->neighbors
      (check-equal? (node->neighbors g node1) (set node2 tag1)))))

;; ======================================================================
;;
;; Node attributes
;;

(define (get-node-attrs g id) ;returns void if node not found
  (hash-ref (graph-ht-node->attrs g) id (void)))

(define (get-node-attr g id k [failure-result (void)])
  (let ([hm (get-node-attrs g id)])
    (if (void? hm)
      (void)
      (hash-ref (get-node-attrs g id) k failure-result))))

(define (set-node-attr g node k v)
  (if (has-node? g node)
    (let ([attrs (get-node-attrs g node)])
      (struct-copy graph g
        [ht-node->attrs
          (hash-set (graph-ht-node->attrs g) node (hash-set attrs k v))]))
    g))

(define (update-node-attr g node k f failure-result)
  (if (has-node? g node)
    (let* ([attrs (get-node-attrs g node)]
           [attrs (hash-update attrs k f failure-result)])
      (struct-copy graph g
        [ht-node->attrs
          (hash-set (graph-ht-node->attrs g) node attrs)]))
    g))

(define (union-node-attrs g node override-attrs)
    (let* ([attrs (get-node-attrs g node)]
           [attrs (hash-union attrs override-attrs #:combine (λ (v0 v) v))])
      (struct-copy graph g
        [ht-node->attrs
          (hash-set (graph-ht-node->attrs g) node attrs)])))

;;TODO Update calling code: argument order is reversed
;; Returns value of id's attribute k, or #f if either node or key not found
(define (node-attr? g node k)
  (let ([ht (get-node-attrs g node)])
    (if (void? ht) #f (hash-ref ht k #f))))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "node attrs"  
    (let*-values ([(g target15) (make-node empty-graph
                                   #hash((name . target15) (class . target)))]
                  [(g target15a) (make-node g
                                   #hash((name . target15) (class .  target)))])
      (check-equal? (get-node-attr g 'target15 'name) 'target15)
      (check-equal? (get-node-attr g 'target15 'class) 'target)
      (check-equal? (get-node-attr g 'target15 'id) 'target15)
      (check-equal? (get-node-attr g 'target15a 'name) 'target15)
      (check-equal? (get-node-attr g 'target15a 'class) 'target)
      (check-equal? (get-node-attr g 'target15a 'id) 'target15a)
      (check-pred void? (get-node-attr g 'no-such-node 'id))
      (check-pred void? (get-node-attr g 'target15 'no-such-attr))
      
      (check-false (node-attr? g 'no-such-node 'name))
      (check-false (node-attr? g 'target15 'no-such-attr))
      (check-not-false (node-attr? g 'target15 'name))

      (let*-values ([(g) (set-node-attr g 'target15 'support 0.64)]
                    [(_) (check-equal? (get-node-attr g 'target15 'support)
                                       0.64)]
                    [(g) (update-node-attr g 'target15
                                             'support
                                             (curry + 0.1)
                                             0.0)]
                    [(_) (check-equal? (get-node-attr g 'target15 'support)
                                       0.74)]
                    [(g) (union-node-attrs g 'target15
                           #hash((tob . 10) (support . 0.4)))]
                    [(_) (check-equal? (get-node-attrs g 'target15)
                           #hash((name . target15) (id . target15)
                                 (class . target) (support . 0.4)
                                 (tob . 10)))])
        (void)))))

;; ======================================================================
;;
;; Graph variables
;; 
;; Graph variables are variables associated with the graph as a whole.
;; Each variable has its own stack, manipulable by graph-push-var and
;; graph-pop-var.
;;
;; It's not an error to pop an empty stack.
;;

(module+ test
  (test-case "graph variables"
    (let* ([g empty-graph]
           ; Setting, getting, and updating
           [g (graph-set-var g 'abc 5)]
           [_ (check-equal? (graph-get-var g 'abc) 5)]
           [_ (check-equal? (graph-get-var g 'undefined 86) 86)]
           [g (graph-update-var g 'abc add1)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           ; Pushing and popping
           [g (graph-push-var g 'abc)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           [g (graph-set-var g 'abc 'new-value)]
           [_ (check-equal? (graph-get-var g 'abc) 'new-value)]
           [g (graph-pop-var g 'abc)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           ; Push and set at the same time
           [g (graph-push-and-set-var g 'xyz 123)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 123)]
           [g (graph-push-and-set-var g 'xyz 456)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 456)]
           [g (graph-pop-var g 'xyz)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 123)]
           [g (graph-pop-var g 'xyz)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) (void))])
      (void))))

(define (graph-set-var g name value)
  (let ([ht (hash-set (graph-vars g) name value)])
    (struct-copy graph g [vars ht])))

(define graph-get-var
  (case-lambda
    [(g name)
     (hash-ref (graph-vars g) name)]
    [(g name failure-result)
     (hash-ref (graph-vars g) name failure-result)]))

(define graph-update-var
  (case-lambda
    [(g name f)
     (let ([ht (hash-update (graph-vars g) name f)])
       (struct-copy graph g [vars ht]))]
    [(g name f failure-result)
     (let ([ht (hash-update (graph-vars g) name f failure-result)])
       (struct-copy graph g [vars ht]))]))

(define (graph-remove-var g name)
  (let ([vars (hash-remove (graph-vars g) name)])
    (struct-copy graph g [vars vars])))

(define (graph-push-var g name)
  (let ([vars (graph-vars g)])
    (if (hash-has-key? vars name)
      (let* ([value (hash-ref vars name)]
             [stacks (hash-update (graph-stacks g)
                                  name
                                  (λ (stack) (cons value stack))
                                  (λ () '()))])
        (struct-copy graph g [stacks stacks]))
      g)))

(define (graph-push-and-set-var g name value)
  (let ([g (graph-push-var g name)])
    (graph-set-var g name value)))

(define (graph-pop-var g name)
  (let ([stacks (graph-stacks g)])
    (cond
      [(not (hash-has-key? stacks name))
       (graph-remove-var g name)]
      [else
       (let* ([stack (hash-ref stacks name)]
              [value (car stack)]
              [stack (cdr stack)]
              [stacks (if (null? stack)
                        (hash-remove stacks name)
                        (hash-set stacks name stack))]
              [vars (hash-set (graph-vars g) name value)])
         (struct-copy graph g [stacks stacks] [vars vars]))])))

;; ======================================================================
;;
;; Copying one graph into another
;; 

(define (map-edge node-map edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (define new-node1 (hash-ref node-map node1))
  (define new-node2 (hash-ref node-map node2))
  `((,new-node1 ,port-label1) (,new-node2 ,port-label2)))

; Returns two values: g, node-map
(define (copy-graph-into-graph g g1)
  (let-values ([(g node-map)
      (for/fold ([g g] [node-map #hash()])
                ([node (all-nodes g1)])
        (let-values ([(g nodeid) (make-node g (get-node-attrs g1 node))])
          (values g (hash-set node-map node nodeid))))])
    (let ([g (for/fold ([g g])
                       ([edge (all-edges g1)])
                (let ([g-edge (map-edge node-map (set->list edge))])
                  (if (has-edge? g g-edge)
                    g
                    (add-edge g g-edge (graph-edge-weight g1 edge)))))])
      (values g node-map))))

(define (copy-graph g)
  (first-value (copy-graph-into-graph (make-empty-graph (graph-spec g))
                                      g)))

(module+ test
  (test-case "copy-graph-into-graph"
    (let*-values ([(g) (add-node empty-graph
                                 #hash((class . letter) (name . a)))]
                  [(g) (add-node g #hash((class . letter) (name . b)))]
                  [(g) (add-edge g '((a out) (b in)))]
                  [(g* node-map) (copy-graph-into-graph empty-graph g)]
                  [(g** node-map2) (copy-graph-into-graph g* g)])
      (check-equal? (list->set (all-nodes g**))
                    (set 'a 'b 'a2 'b2))
      (check-equal? (list->set (all-edges g**))
                    (set (set '(a out) '(b in))
                         (set '(a2 out) '(b2 in)))))))

;; ======================================================================
;;
;; Commonly useful specialized graph functions
;;

(define (members-of g groupid)
  (port->neighbors g `(,groupid members)))

(define (member-of g node)
  (port->neighbors g `(,node member-of)))

(define (member-of? g ctx node)
  (member ctx (member-of g node)))

(define/g (no-neighbor-at-port? g port-label node)
  (null? (port->neighbors g `(,node ,port-label))))

(define/g (has-neighbor-at-port? g port-label node)
  (not (no-neighbor-at-port? g port-label node)))

;; ======================================================================
;;
;; Printing a graph
;; 

(define (pr-node g nodeid)
  (if (has-node? g nodeid)
    (begin
      (printf " ~a ~a\n" (~a nodeid #:min-width 12)
                          (hash-remove* (get-node-attrs g nodeid)
                                        'id 'node->actions))
      (for* ([port (node->ports g nodeid)]
             [neighboring-port (port->neighboring-ports g port)])
        (define weight (~r (graph-edge-weight g `(,port ,neighboring-port))
                           #:precision '(= 1)))
        (printf "  ~a -- ~a ~a\n" port neighboring-port weight)))
    (printf "No such node: ~a\n" nodeid))) 

(define (pr-graph g)
  (displayln "nodes:")
  (for ([nodeid (sort (set->list (all-nodes g))
                      (λ (id1 id2) (string<? (~a id1) (~a id2))))])
    (pr-node g nodeid)))

(define (pr-group g groupid)
  (displayln "nodes:")
  (for ([nodeid (cons groupid
                      (sort (members-of g groupid)
                            (λ (id1 id2) (string<? (~a id1) (~a id2)))))])
    (pr-node g nodeid)))
