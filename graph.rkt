; graph.rkt -- Data structure for port graphs
;
; This is the lowest-level representation of a graph in FARGish. The make-node
; function performs no initialization, checking, tracking, or updating of
; salience. It simply gives the new node whatever attributes it is passed.

#lang debug at-exp typed/racket

(require "types.rkt"
         "id-set.rkt")
(require "typed-wheel.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(module+ test (require typed/rackunit phc-toolkit/typed-rackunit))

(struct Graph ([ht-node->attrs : (Hashof Node Attrs)]
               [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
               [ht-edge->weight : (Hashof Edge/UPair EdgeWeight)]
               [id-set : IdSet]
               [stacks : (Hashof Symbol (Listof Any))]
               [vars : (Hashof Symbol Any)])
              #:prefab)

(define (make-empty-graph)  ; TODO spec
  (Graph (hash) (hash) (hash) empty-id-set (hash) (hash)))

(define empty-graph (make-empty-graph))

(define empty-port-set : (Setof Port) (set))
(define empty-node-set : (Setof Node) (set))

(: E : Port Port -> Edge/UPair)  ; Edge constructor
(define E UnorderedPair)


;(define-struct/exec Gfunc () 

; A little hack to make it easier to work on graphs in the REPL.
; (gdo makenode 'ws) operates on a variable in the local context called g,
; set!s g to the new graph, and returns the nodeid of the created node.
(define-syntax (gdo stx)
  (syntax-case stx ()
    [(gdo gfunc args ...)
     (with-syntax ([g (format-id #'gdo "g"
                                 #:source #'gdo #:props #'f)])
       #'(call-with-values (λ () (gfunc g args ...))
           (λ ([new-g : Graph] . results)
             (set! g new-g)
             (cond
               [(null? results) (void)]
               [(null? (cdr results)) (car results)]
               [else results]))))]))

(define g (make-empty-graph))
(set! g g)

; TODO Copy over test cases from graph1.rkt

;; ======================================================================
;;
;; Making and removing nodes
;;

(: make-node : Graph Attrs -> (Values Graph Node))
(define (make-node g attrs)
  (let*-values ([(attrs name) (ensure-node-has-name g attrs)]
                [(id-set id) (gen-id (Graph-id-set g) name)]
                [(attrs) (hash-set attrs 'id id)]
                [(g) (let ([ht (Graph-ht-node->attrs g)])
                       (struct-copy Graph g
                         [ht-node->attrs (hash-set ht id attrs)]
                         [id-set id-set]))])
    (values g id)))

(: add-node : Graph Attrs -> Graph)
(define (add-node g attrs)
  (first-value (make-node g attrs)))

(: remove-node : Graph Node -> Graph)
(define (remove-node g node)
  (let ([g (for/fold ([g : Graph g])
                     ([edge (node->edges g node)])
             (remove-edge g edge))])
    (struct-copy Graph g
      [ht-node->attrs (hash-remove (Graph-ht-node->attrs g) node)])))

(: ensure-node-has-name : Graph Attrs -> (Values Attrs DisplayName))
(define (ensure-node-has-name g attrs)
  (: set-name-and-return : Any -> (Values Attrs DisplayName))
  (define (set-name-and-return name)
    (let ([name (->display-name name)])
      (values (hash-set attrs 'name name) name)))
  (cond
    [(hash-ref attrs 'name #f)
     => (λ (name) (values attrs (->display-name name)))]
    [(hash-ref attrs 'value #f)
     => set-name-and-return]
    [(hash-ref attrs 'class #f)
     => set-name-and-return]
    [else (set-name-and-return 'UNKNOWN)]))

(: ->display-name : Any -> DisplayName)
(define (->display-name x)
  (cond
    [(symbol? x) x]
    [(string? x) x]
    [(integer? x) x]
    [(void? x) "(void)"]
    [else (~a x)]))

;; ======================================================================
;;
;; Making and removing edges
;;

(: edge->upair : Edge -> Edge/UPair)
(define (edge->upair edge)
  (cond
    [(edge/upair? edge) edge]
    [(list? edge) (list->upair edge)]))

(: edge->list : Edge -> Edge/List)
(define (edge->list edge)
  (cond
    [(list? edge) edge]
    [(edge/upair? edge) (upair->list edge)]))

(: add-edge (->* (Graph Edge) (EdgeWeight) Graph))
(define (add-edge g edge [weight 1.0])
  (let ([edge (edge->list edge)])
    (match-define `(,port1 ,port2) edge)
    (define edges (Graph-ht-edge->weight g))
    (let* ([p->nps (Graph-ht-port->neighboring-ports g)]
           [p->nps (hash-update p->nps
                                port1
                                (λ ([st : (Setof Port)]) (set-add st port2))
                                (const empty-port-set))]
           [p->nps (hash-update p->nps
                                port2
                                (λ ([st : (Setof Port)]) (set-add st port1))
                                (const empty-port-set))])
      (struct-copy Graph g
        [ht-edge->weight (hash-set edges (E port1 port2) weight)]
        [ht-port->neighboring-ports p->nps]))))

(: remove-edge : Graph Edge -> Graph)
(define (remove-edge g e)
  (define edge (edge->list e))
  (match-define `(,port1 ,port2) edge)
  (define edge* (edge->upair e))
  (let* ([p->nps (Graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps
                              port1
                              (λ ([st : (Setof Port)]) (set-remove st port2))
                              (const empty-port-set))]
         [p->nps (hash-update p->nps
                              port2
                              (λ ([st : (Setof Port)]) (set-remove st port1))
                              (const empty-port-set))])
    (struct-copy Graph g
      [ht-edge->weight (hash-remove (Graph-ht-edge->weight g) edge*)]
      [ht-port->neighboring-ports p->nps])))

;; ======================================================================
;;
;; Querying existence of nodes and edges
;;

(: all-nodes : Graph -> (Listof Node))
(define (all-nodes g)
  (hash-keys (Graph-ht-node->attrs g)))

(: all-edges : Graph -> (Listof Edge/UPair))
(define (all-edges g)
  (hash-keys (Graph-ht-edge->weight g)))

(: has-node? : Graph Node -> Boolean)
(define (has-node? g id)
  (hash-has-key? (Graph-ht-node->attrs g) id))

(: has-edge? : Graph Edge -> Boolean)
(define (has-edge? g edge)
  (hash-has-key? (Graph-ht-edge->weight g) (edge->upair edge)))

;; ======================================================================
;;
;; Edge weight
;;

(: edge->weight (->* [Graph Edge] [EdgeWeight] EdgeWeight))
(define (edge->weight g edge [weight-if-no-edge (void)])
  (let ([edge (edge->upair edge)])
    (hash-ref (Graph-ht-edge->weight g) edge (const weight-if-no-edge))))

; If edge does not exist, we don't create it.
(: set-edge-weight : Graph Edge EdgeWeight -> Graph)
(define (set-edge-weight g edge weight)
  (let ([edge (edge->upair edge)])
    (if (has-edge? g edge)
      (struct-copy Graph g
        [ht-edge->weight (hash-set (Graph-ht-edge->weight g) edge weight)])
      g)))

;; ======================================================================
;;
;; Extracting elements of ports and edges
;;

(: port->node : Port -> Node)
(define port->node car)

(: port->port-label : Port -> Port-label)
(define port->port-label cadr)

(: port->values : Port -> (Values Node Port-label))
(define (port->values port)
  (values (car port) (cadr port)))

(: edge->nodes : Edge -> (List Node Node))
(define (edge->nodes edge)
  (let ([edge (edge->list edge)])
    (list (port->node (first edge)) (port->node (second edge)))))

(: hop->to-node : Hop -> Node)
(define hop->to-node caadr)

(: hop->from-node : Hop -> Node)
(define hop->from-node caar)

;; ======================================================================
;;
;; Neighbors
;;

(: port->neighboring-ports : Graph Port -> (Setof Port))
(define (port->neighboring-ports g port)
  (define p->nps (Graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port (const empty-port-set)))

; Returns up to one port that neighbors port, chosen arbitrarily.
(: port->neighboring-port : Graph Port -> (U Port Void))
(define (port->neighboring-port g port)
  (let ([nports (port->neighboring-ports g port)])
    (cond
      [(null? nports) (void)]
      [else (car (set->list nports))])))

(: port->neighbors : Graph Port -> (Listof Node))
(define (port->neighbors g port)
  (for/list ([nport : Port (port->neighboring-ports g port)])
    (port->node nport)))

; Returns up to one neighbor of port, chosen arbitrarily.
(: port->neighbor : Graph Port -> (U Node Void))
(define (port->neighbor g port)
  (let ([neighbors (port->neighbors g port)])
    (cond
      [(null? neighbors) (void)]
      [else (car neighbors)])))

(: port-neighbor? : Graph Port Node -> Boolean)
(define (port-neighbor? g port node)
  (for/or ([neighbor (port->neighbors g port)])
    (equal? neighbor node)))

(: port-has-neighbor? : Graph Port -> Boolean)
(define (port-has-neighbor? g port)
  (not (set-empty? (port->neighboring-ports g port))))

; Returns all neighbors playing role in relation to node.
; A "role" is the port-label by which the neighbor connects back to node,
; i.e. its role in relation to node. For example, from the standpoint
; of a node 3, linked to 6 by an edge `((3 less-than) (6 greater-than)),
; the 6 is playing the role "greater-than" in relation to the 3.
(: port->neighbors/role : Graph Port Port-label -> (Setof Node))
(define (port->neighbors/role g port role)
  (for/fold ([nodes empty-node-set])
            ([nport : Port (port->neighboring-ports g port)])
    (let-values ([(nnode nport-label) (port->values nport)])
      (if (equal? role nport-label)
        (set-add nodes nnode)
        nodes))))

(: port->incident-edges : Graph Port -> (Listof Edge/UPair))
(define (port->incident-edges g port)
  (for/list ([nport : Port (port->neighboring-ports g port)])
    (E port nport)))

;TODO UT
;Returns a list of edges, each edge represented as a list, with port first
(: port->incident-hops : Graph Port -> (Listof Hop))
(define (port->incident-hops g port)
  (for/list ([nport : Port (port->neighboring-ports g port)])
    (list port nport)))

(: other-node : Hop -> Node)
(define other-node caadr)

(: node->neighbors : Graph Node -> (Setof Node))
(define (node->neighbors g node)
  (for*/set : (Setof Node)
    ([port : Port (node->ports g node)]
     [neighbor : Node (port->neighbors g port)])
      neighbor))

(: node->edges : Graph Node -> (Listof Edge/UPair))
(define (node->edges g node)
  (for*/list : (Listof Edge/UPair)
    ([port : Port (node->ports g node)]
     [nport : Port (port->neighboring-ports g port)])
      (E port nport)))

;TODO Inefficient
(: node->ports : Graph Node -> (Listof Port))
(define (node->ports g node)
  (for/list : (Listof Port)
    ([port : Port (hash-keys (Graph-ht-port->neighboring-ports g))]
     #:when (equal? node (car port)))
      port))

; The first port of each hop is node's port.
(: node->incident-hops : Graph Node -> (Listof Hop))
(define (node->incident-hops g node)
  (for*/list : (Listof Hop)
    ([port : Port (node->ports g node)]
     [nport : Port (port->neighboring-ports g port)])
      `(,port ,nport)))

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
      (check-equal? (edge->weight g '((plus operand) (source9 output)))
                    1.0)
      (let* ([g (remove-edge g '((plus operand) (source9 output)))])
        (check-false (has-edge? g '((source9 output) (plus operand)))))))

  (let* ([ab-graph (add-node empty-graph #hash((class . letter) (name . a)))]
         [ab-graph (add-node ab-graph #hash((class . letter) (name . b)))])
    (test-case "add-edge with weight"
      (let ([g (add-edge ab-graph '((a out) (b in)) 0.62)])
        (check-equal? (edge->weight g '((a out) (b in)))
                      0.62)))
    (test-case "add-edge twice"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.51)]
             [g (add-edge g '((a out) (b in)))])
        (check-equal? (list (E '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (edge->weight g `((a out) (b in)))
                      1.00)))
    (test-case "add-edge twice with weight"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.75)]
             [g (add-edge g '((a out) (b in)) 0.22)])
        (check-equal? (list (E '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (edge->weight g `((a out) (b in)))
                      0.22)))
    (test-case "set-edge-weight"
      (let* ([_ (check-pred void? (edge->weight g '((a out) (b in))))]
             [g (set-edge-weight g '((a out) (b in)) 0.4)]
             ; edge does not exist, so no effect
             [_ (check-pred void? (edge->weight g '((a out) (b in))))]
             [g (add-edge g '((a out) (b in)) 0.6)]
             [_ (check-equal? (edge->weight g '((a out) (b in))) 0.6)]
             [g (set-edge-weight g '((a out) (b in)) 0.4)]
             [_ (check-equal? (edge->weight g '((a out) (b in))) 0.4)])
        (void))))
  
  (test-case "port-> various neighbor functions"
    (let*-values ([(g node1) (make-node empty-graph #hash((name . node1)))]
                  [(g node2) (make-node g #hash((name . node2)))]
                  [(g node3) (make-node g #hash((name . node3)))]
                  [(g) (add-edge g `((,node1 from-port) (,node2 to-port)))]
                  [(g) (add-edge g `((,node1 from-port) (,node3 to-port)))])
      ; port->neighboring-ports
      (check-equal? (port->neighboring-ports g `(,node1 from-port))
                    (set `(,node2 to-port) `(,node3 to-port)))
      (check-equal? (port->neighboring-ports g `(,node2 to-port))
                    (set `(,node1 from-port)))
      (check-equal? (port->neighboring-ports g `(,node3 to-port))
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
                    (set (E `(,node1 from-port) `(,node2 to-port))
                         (E `(,node1 from-port) `(,node3 to-port))))))

  (test-case "port->neighbors/role"
    (let*-values ([(g node1) (make-node empty-graph #hash((name . node1)))]
                  [(g tag1) (make-node g #hash((name . tag1)))]
                  [(g tag2) (make-node g #hash((name . tag2)))]
                  [(g tag3) (make-node g #hash((name . tag3)))]
                  [(g) (add-edge g `((,node1 tags) (,tag1 taggee-type-a)))]
                  [(g) (add-edge g `((,node1 tags) (,tag2 taggee-type-a)))]
                  [(g) (add-edge g `((,node1 tags) (,tag3 taggee-type-b)))])
      (check-equal? (port->neighbors/role g `(,node1 tags) 'taggee-type-a)
                    (set tag1 tag2))
      (check-equal? (port->neighbors/role g `(,node1 tags) 'taggee-type-b)
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

(: get-node-attrs : Graph Node -> (U Attrs Void))
(define (get-node-attrs g id)
  (hash-ref (Graph-ht-node->attrs g) id (const (void))))

(: get-node-attr (->* [Graph Node Any] [Any] (U Any Void)))
(define (get-node-attr g id k [failure-result (void)])
  (let ([ht (get-node-attrs g id)])
    (if (void? ht)
      (void)
      (hash-ref ht k (const failure-result)))))

; No effect if node does not exist; node is not created.
(: set-node-attr : Graph Node Any Any -> Graph)
(define (set-node-attr g node k v)
  (let ([attrs (get-node-attrs g node)])
    (cond
      [(void? attrs) g]
      [else
        (let* ([attrs (hash-set attrs k v)]
               [ht-node->attrs (hash-set (Graph-ht-node->attrs g) node attrs)])
          (struct-copy Graph g
            [ht-node->attrs ht-node->attrs]))])))

; No effect if node does not exist; node is not created.
;(: update-node-attr (->* [Graph Node Any (Any -> Any)] [(-> Any)] Graph))
;(: update-node-attr (All (V) Graph Node Any (V -> V) (-> V) -> Graph))
(: update-node-attr : Graph Node Any (Any -> Any) (-> Any) -> Graph)
(define (update-node-attr g node k f failure-result)
  (cond
    #:define attrs (get-node-attrs g node)
    [(void? attrs) g]
    #:define attrs (hash-update attrs k f failure-result)
    [else (struct-copy Graph g
            [ht-node->attrs
              (hash-set (Graph-ht-node->attrs g) node attrs)])]))

; No effect if node does not exist; node is not created.
(: merge-node-attrs : Graph Node Attrs -> Graph)
(define (merge-node-attrs g node override-attrs)
  (cond
    #:define attrs (get-node-attrs g node)
    [(void? attrs) g]
    #:define attrs (hash-merge attrs override-attrs)
    [else (struct-copy Graph g
            [ht-node->attrs
              (hash-set (Graph-ht-node->attrs g) node attrs)])]))

;;TODO Update calling code: argument order is reversed
;; Returns value of id's attribute k, or #f if either node or key not found
(: node-attr? : Graph Node Any -> (U Any #f))
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
                                             (cast (curry+ 0.1) (Any -> Any))
                                             (const 0.0))]
                    [(_) (check-equal? (get-node-attr g 'target15 'support)
                                       0.74)]
                    [(g) (merge-node-attrs g 'target15
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
    (: add1/any : Any -> Any)
    (define (add1/any x)
      (add1 (cast x Number)))

    (let* ([g empty-graph]
           ; Setting, getting, and updating
           [g (graph-set-var g 'abc 5)]
           [_ (check-equal? (graph-get-var g 'abc) 5)]
           [_ (check-equal? (graph-get-var g 'undefined 86) 86)]
           [g (graph-update-var g 'abc add1/any 0)]
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

(: graph-set-var : Graph Symbol Any -> Graph)
(define (graph-set-var g name value)
  (let ([ht (hash-set (Graph-vars g) name value)])
    (struct-copy Graph g [vars ht])))

(: graph-get-var (case-> (Graph Symbol -> Any)
                         (Graph Symbol Any -> Any)))
(define graph-get-var
  (case-lambda
    [(g name)
     (hash-ref (Graph-vars g) name)]
    [(g name failure-result)
     (hash-ref (Graph-vars g) name (const failure-result))]))

(: graph-update-var : Graph Symbol (Any -> Any) Any -> Graph)
(define graph-update-var
  (case-lambda
    [(g name f failure-result)
     (let ([ht (hash-update (Graph-vars g) name f (const failure-result))])
       (struct-copy Graph g [vars ht]))]))

(: graph-remove-var : Graph Symbol -> Graph)
(define (graph-remove-var g name)
  (let ([vars (hash-remove (Graph-vars g) name)])
    (struct-copy Graph g [vars vars])))

(: graph-push-var : Graph Symbol -> Graph)
(define (graph-push-var g name)
  (let ([vars (Graph-vars g)])
    (if (hash-has-key? vars name)
      (let* ([value (hash-ref vars name)]
             [stacks (hash-update (Graph-stacks g)
                                  name
                                  (λ ([stack : (Listof Any)])
                                    (cons value stack))
                                  (λ () '()))])
        (struct-copy Graph g [stacks stacks]))
      g)))

(: graph-push-and-set-var : Graph Symbol Any -> Graph)
(define (graph-push-and-set-var g name value)
  (let ([g (graph-push-var g name)])
    (graph-set-var g name value)))

(: graph-pop-var : Graph Symbol -> Graph)
(define (graph-pop-var g name)
  (let ([stacks (Graph-stacks g)])
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
              [vars (hash-set (Graph-vars g) name value)])
         (struct-copy Graph g [stacks stacks] [vars vars]))])))

;; ======================================================================
;;
;; Copying one graph into another
;; 

(: map-edge : (Hashof Node Node) Edge/List -> Edge/List)
(define (map-edge node-map edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (define new-node1 (hash-ref node-map node1))
  (define new-node2 (hash-ref node-map node2))
  `((,new-node1 ,port-label1) (,new-node2 ,port-label2)))

(: copy-nodes : Graph Graph -> (Values Graph (Hashof Node Node)))
(define (copy-nodes g g1)  ; copies from g1 into g
  (for/fold : (Values Graph (Hashof Node Node))
            ([g g] [node-map : (Hashof Node Node) #hash()])
            ([node (all-nodes g1)])
    (let-values ([(g nodeid) (make-node g
                               (cast (get-node-attrs g1 node) Attrs))])
      (values g (hash-set node-map node nodeid)))))

(: copy-edges : Graph Graph (Hashof Node Node) -> Graph)
(define (copy-edges g g1 node-map)  ; copies from g1 into g
  (for/fold : Graph
            ([g g])
            ([edge (all-edges g1)])
    (let ([g-edge (map-edge node-map (edge->list edge))])
      (if (has-edge? g g-edge)
        g
        (add-edge g g-edge (edge->weight g1 edge))))))

; Returns two values: g, node-map
(: copy-graph-into-graph : Graph Graph -> (Values Graph (Hashof Node Node)))
(define (copy-graph-into-graph g g1)
  (let*-values ([(g node-map) (copy-nodes g g1)]
                [(g) (copy-edges g g1 node-map)])
    (values g node-map)))

(: copy-graph : Graph -> Graph)
(define (copy-graph g)
  (first-value (copy-graph-into-graph (make-empty-graph #;(graph-spec g))
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
                    (set (E '(a out) '(b in))
                         (E '(a2 out) '(b2 in)))))))
