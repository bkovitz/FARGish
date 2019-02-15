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
(module+ test (require rackunit))

(define-type Attrs (Hashof Any Any))

(struct Graph ([ht-node->attrs : (Hashof Node Attrs)]
               [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
               [edges : (Hashof Edge/Set EdgeWeight)]
               [id-set : IdSet]
               ;[stacks : ??]
               [vars : (Hashof Symbol Any)])
              #:prefab)

(define (make-empty-graph)  ; TODO spec
  (Graph (hash) (hash) (hash) empty-id-set (hash)))

(define empty-graph (make-empty-graph))

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

(: edge->set : Edge -> Edge/Set)
(define (edge->set edge)
  (cond
    [(set? edge) edge]
    [(pair? edge) (list->set edge)]))

(: edge->list : Edge -> Edge/List)
(define (edge->list edge)
  (cond
    [(pair? edge) edge]
    [(set? edge) (set->list edge)])) ;PROBLEM Type checker doesn't know what's
                                     ;in the set. SOLUTION? Structs, not
                                     ;sets, with a custom equality func.

(: remove-edge : Graph Edge -> Graph)
(define (remove-edge g e)
  (define edge (edge->list e))
  (match-define `(,port1 ,port2) edge)
  (define edge* (edge->set e))
  (let* ([p->nps (Graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps
                              port1
                              (λ (st) (set-remove st port2))
                              (set))]
         [p->nps (hash-update p->nps
                              port2
                              (λ (st) (set-remove st port1))
                              (set))])
    (struct-copy Graph g
      [edges (hash-remove (Graph-edges g) edge*)]
      [ht-port->neighboring-ports p->nps])))

;; ======================================================================
;;
;; Neighbors
;;

(: port->neighboring-ports : Graph Port -> (Setof Port))
(define (port->neighboring-ports g port)
  (define p->nps (Graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port (set)))

(: node->edges : Graph Node -> (Listof Edge/Set))
(define (node->edges g node)
  (for*/list : (Listof Edge/Set)
    ([port (node->ports g node)]
     [nport (port->neighboring-ports g port)])
      (set port nport)))

;TODO Inefficient
(: node->ports : Graph Node -> (Listof Port))
(define (node->ports g node)
  (for/list ([port (hash-keys (Graph-ht-port->neighboring-ports g))]
             #:when (equal? node (car port)))
    port))

; The first port of each hop is node's port.
(: node->incident-hops : Graph Node -> (Listof Hop))
(define (node->incident-hops g node)
  (for*/list : (Listof Hop)
    ([port (node->ports g node)]
     [nport (port->neighboring-ports g port)])
      `(,port ,nport)))
