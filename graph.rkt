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
(module+ test (require typed/rackunit))

(define-type Attrs (Hashof Any Any))

(struct Graph ([ht-node->attrs : (Hashof Node Attrs)]
               [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
               [ht-edge->weight : (Hashof Edge/UPair EdgeWeight)]
               [id-set : IdSet]
               ;[stacks : ??]
               [vars : (Hashof Symbol Any)])
              #:prefab)

(define (make-empty-graph)  ; TODO spec
  (Graph (hash) (hash) (hash) empty-id-set (hash)))

(define empty-graph (make-empty-graph))

(define empty-port-set : (Setof Port) (set))

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
;; Neighbors
;;

(: port->neighboring-ports : Graph Port -> (Setof Port))
(define (port->neighboring-ports g port)
  (define p->nps (Graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port (const empty-port-set)))

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
        (void)))))
