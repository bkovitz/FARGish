; graph1.rkt -- Data structure for port graphs

#lang debug at-exp racket

(require "wheel.rkt" "sigs.rkt" "id-set.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require racket/hash)
(require rackunit racket/pretty describe debug/repl racket/enter)

;(require rackunit racket/generic racket/struct "id-set.rkt"
;         racket/dict racket/pretty describe mischief/memoize) 
;(require racket/serialize)

; TEMPORARY version: replace with graph-core^ from sigs.rkt.
(define-signature graph-core1^
  (empty-graph
   graph?

   make-node  ; g attrs -> g nodeid
   ;remove-node
   add-node
   add-edge
   
   has-node?
   has-edge?
   graph-edge-weight
   ;all-nodes
   ;all-edges

   ;port->neighboring-ports
   ;port->neighbors
   ;port->neighbor
   ;port->port-label->nodes 
   ;port->incident-edges

   ;node->neighbors
   ;node->ports
   ;node->incident-edges

   ;define/g
   ;gdo
   ))

(define-signature graph-name^
  (ensure-node-name)) ; g attrs -> attrs name

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
  (export graph-core1^)

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

  #;(define (remove-node g node)
    (let ([g (for/fold ([g g])
                       ([edge (node->incident-edges g node)])
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

  ;; ----------------------------------------------------------------------
  ;;
  ;; Querying existence of nodes and edges
  ;;

  (define (has-node? g id)
    (hash-has-key? (graph-ht-node->attrs g) id))

  (define (has-edge? g edge)
    (hash-has-key? (graph-edges g) (edge->set edge)))

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

  )

(module+ test
  (define-compound-unit/infer g@
    (import)
    (export graph-name^ graph-core1^)
    (link simple-graph-name@ graph-core@))

  (define-values/invoke-unit g@ (import) (export graph-core1^))

  (test-case "make-node"
    (let*-values ([(g) empty-graph]
                  [(_) (check-false (has-node? g 5))]
                  [(g id1) (make-node g #hash((value . 5)))]
                  [(_) (check-equal? id1 5)]
                  [(_) (check-true (has-node? g 5))])
      (void)))

  (test-case "add-edge"
    (let* ([g (add-node empty-graph #hash((class . number) (name . source9)))]
           [g (add-node g #hash((class . plus)))]
           [g (add-edge g '((source9 output) (plus operand)))])
      (check-true (has-edge? g '((source9 output) (plus operand))))
      (check-true (has-edge? g '((plus operand) (source9 output))))
      (check-equal? (graph-edge-weight g '((plus operand) (source9 output)))
                    1.0)
      #;(let* ([g (remove-edge g '((plus operand) (source9 output)))])
        (check-false (has-edge? g '((source9 output) (plus operand)))))))

  )
