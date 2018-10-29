; graph1.rkt -- Data structure for port graphs

#lang debug at-exp racket

(require "wheel.rkt" "sigs.rkt" "id-set.rkt" "sigs.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require racket/hash)
(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe debug/repl racket/enter)

;(require rackunit racket/generic racket/struct "id-set.rkt"
;         racket/dict racket/pretty describe mischief/memoize) 
;(require racket/serialize)

;; ======================================================================
;;
;; simple-graph-name@
;;

(define-unit simple-graph-name@
  (import)
  (export graph-name^)

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
      [else (set-name-and-return 'UNKNOWN)])))

;; ======================================================================
;;
;; graph-core@
;;

(define-unit graph-core@
  (import graph-name^)
  (export graph-core^)

  (struct graph (ht-node->attrs
                 ht-port->neighboring-ports
                 edges
                 id-set
                 stacks  ; dict of temp vars for do-graph-edits
                 vars    ; hash-table of vars: name -> value
                 spec) #:prefab)

  (define empty-spec #hash())
  (define empty-graph
    (graph #hash() #hash() #hash() empty-id-set #hash() #hash() empty-spec))

  ;; ----------------------------------------------------------------------
  ;;
  ;; Making and removing nodes
  ;;

  ; Returns two values: g nodeid
  (define (make-node g attrs)
    (let*-values ([(attrs name) (ensure-node-name g attrs)]
                  [(id-set id) (gen-id (graph-id-set g) name)]
                  [(attrs) (hash-set attrs 'id id)]
                  [(g) (let ([ht (graph-ht-node->attrs g)])
                         (struct-copy graph g
                           [ht-node->attrs (hash-set ht id attrs)]
                           [id-set id-set]))])
      (values g id)))

  (define (add-node . args)
    (first-value (apply make-node args)))

  (define (remove-node g node)
    (let ([g (for/fold ([g g])
                       ([edge (node->incident-hops g node)])
               (remove-edge g edge))])
      (struct-copy graph g
        [ht-node->attrs (hash-remove (graph-ht-node->attrs g) node)])))

  ;; ----------------------------------------------------------------------
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
  ;Doesn't add the edge if it already exists, but will change its weight.
  (define (add-edge g edge [weight 1.0])
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
  (define (remove-edge g e)
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

  ;; ----------------------------------------------------------------------
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

  (define (graph-edge-weight g edge)
    (let ([edge (if (set? edge) (set->list edge) edge)])
      (match-define `(,port1 ,port2) edge)
      (define edge* (set port1 port2))
      (hash-ref (graph-edges g) edge* (void))))

  ;; ----------------------------------------------------------------------
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

  ;Returns set of nodes
  (define (node->neighbors g node)
    (for*/set ([port (node->ports g node)]
               [neighbor (port->neighbors g port)])
      neighbor))
  )

(module+ test
  (define-compound-unit/infer g@
    (import)
    (export graph-name^ graph-core^)
    (link simple-graph-name@ graph-core@))

  (define-values/invoke-unit g@ (import) (export graph-core^))

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
        (check-false (has-edge? g '((source9 output) (plus operand)))))
      ))

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
      (check-equal? (node->neighbors g node1) (set node2 tag1))))
  )
