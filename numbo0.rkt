#lang debug at-exp racket

;HACK TODO
; bind/complete archetype in one step  DONE
; tag scout
; slipnet search
; large number or diff attracts mult
; smaller target attracts subtraction
; done?
; printing the result

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe "graph.rkt")

(define (could-bind? g from-node to-node)
  (or (equal? (value-of g from-node) (value-of g to-node))
      (equal? (class-of g from-node) (class-of g to-node))))

(define (bdx-scout? g from-ctx from-node to-ctx)
  ;search intersection of port-neighborss
  'STUB ;NEXT
  )

(define (start-bdx-scout g from-ctx from-node to-ctx)
  `(:make
     (:define scout (:node bdx-scout))
     (:edge (scout from-node) (,from-node general-port))
     (:edge (scout from-ctx) (,from-ctx general-port))
     (:edge (scout to-ctx) (,to-ctx general-port))))

(define (run-bdx-scout g scout)
  (define from-node (port->neighbor g `(,scout from-node)))
  (define to-ctx (port->neighbor g `(,scout to-ctx)))
  (define to-node (for/first ([to-node (members-of g to-ctx)]
                              #:when (could-bind? g from-node to-node)
                              #:when (not (bound-to? g from-node to-node)))
                    to-node))
  (if to-node
    `(bind ,from-node ,to-node)  ;TODO another agent must build the edges
    `(:make
       (:define new-node (copy-node ,from-node ,to-ctx))
       (bind ,from-node new-node))))

(define (finish-archetype-instantiation g from-ctx to-ctx)
  (define deltas (for/list ([from-node (members-of g from-ctx)]
                             #:when (and (not (bound-from-ctx-to-ctx? g
                                                from-ctx to-ctx from-node))
                                         (not (bdx-scout? g
                                                from-ctx to-ctx from-node))))
                   (start-bdx-scout g from-ctx from-node to-ctx)))
  `(:make ,@deltas))

(define (superficial-matches g from-ctx from-node to-ctx)
  (for/list ([to-node (members-of g to-ctx)]
             #:when (equal? (value-of g from-node) (value-of g to-node)))
    to-node))

(define (build/bind-actions g from-ctx from-node to-ctx)
  `((build ,from-node) ,@(for/list ([to-node (superficial-matches g
                                               from-ctx from-node to-ctx)])
                           `(bind ,from-node ,to-node))))

;TODO filter out completions that don't build anything
(define (all-possible-archetype-completions g from-ctx to-ctx)
  (local-require (only-in racket/list cartesian-product))
  (apply cartesian-product
    (for/list ([from-node (members-of g from-ctx)])
      (build/bind-actions g from-ctx from-node to-ctx))))

(define (attrs-to-copy g from-node)
  (define attrs (get-node-attrs g from-node))
  (define c (hash-ref attrs 'class (void)))
  (define v (hash-ref attrs 'value (void)))
  `((class . ,c) (value . ,v)))

(define (do-bdx-actions g from-ctx to-ctx bdx-actions)
  (for/fold ([g g] [hm-bdx (hash)])
            ([bdx-action bdx-actions])
    (match bdx-action
      [`(bind ,from-node ,to-node)
       (values g (hash-set hm-bdx from-node to-node))]
      [`(build ,from-node)
       (let*-values ([(g to-id) (make-node g (attrs-to-copy g from-node))]
                     [(g) (add-edge g `((,to-id member-of) (,to-ctx members)))])
         (values g (hash-set hm-bdx from-node to-id)))]
      [_ (error "try-bdx-actions")])))

(define (max-neighbors-of g port)
  (match-define `(,node ,port-label) port)
  (case port-label
    [(operands) 2] ;model-specific HACK
    [else 1]))

(define (open-port? g port)
  (< (length (port->neighboring-ports g port))
     (max-neighbors-of g port)))

; throws 'cant-make-edge
(define (try-to-make-edge g edge)
  (match-define `(,port1 ,port2) edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (cond
    [(has-edge? g edge)
     g]
    [(and (open-port? g port1) (open-port? g port2))
     (add-edge g edge)]
    [else (raise 'cant-make-edge)]))

; assumes that edge is a list in a certain order
(define (edge-leads-to-node? g edge ctx)
  (match edge
    [`((,_ ,_) (,to-node ,port-label))
      (equal? to-node ctx)]
    ))

(define (required-edges-in-ctx g node ctx)
  (for/list ([edge (node->incident-edges g node)]
             #:when (not (edge-leads-to-node? g edge ctx)))
    edge))
         
; throws 'cant-make-edge
(define (try-bdx-actions g0 from-ctx to-ctx bdx-actions)
  (let-values ([(g hm-bdx) (do-bdx-actions g0 from-ctx to-ctx bdx-actions)])
    (for*/fold ([g g])
               ([from-node (hash-keys hm-bdx)]
                [from-edge (required-edges-in-ctx g0 from-node from-ctx)])
      (match-define `((,ignored ,port-label1) (,from-neighbor ,port-label2))
                    from-edge)
      (define bindee1 (hash-ref hm-bdx from-node))
      (define bindee2 (hash-ref hm-bdx from-neighbor))
      (try-to-make-edge g `((,bindee1 ,port-label1) (,bindee2 ,port-label2)))
      )))

(define (cant-make-edge? x)
  (eq? 'cant-make-edge x))

(define (build? x)
  (and (list? x)
       (eq? 'build (car x))))

(define (best-completion actions->g)
  (cdr
    (argmin (λ (ag) (count build? (car ag)))
            actions->g)))

(define (hacked-finish-archetype g from-ctx to-ctx)
  (define actions->g
    (for/fold ([alist '()])
              ([bdx-actions (all-possible-archetype-completions g
                              from-ctx to-ctx)])
      (with-handlers ([cant-make-edge? (λ (x) alist)])
        (cons `(,bdx-actions . ,(try-bdx-actions g from-ctx to-ctx bdx-actions))
              alist))))
  (best-completion actions->g))

;; Making a slipnet

(define (map-edge node-map edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (define new-node1 (dict-ref node-map node1))
  (define new-node2 (dict-ref node-map node2))
  `((,new-node1 ,port-label1) (,new-node2 ,port-label2)))

(define (copy-graph-into-graph g g1)
  (let-values ([(g node-map)
      (for/fold ([g g] [node-map '()])
                ([node (all-nodes g1)])
        (let-values ([(g nodeid) (make-node g (get-node-attrs g1 node))])
          (values g (cons `(,node . ,nodeid) node-map))))])
    (let ([g (for/fold ([g g])
                        ([edge (all-edges g1)])
                (let ([edge (map-edge node-map (set->list edge))])
                  (if (has-edge? g edge) g (add-edge g edge))))])
      (values g (dict-values node-map)))))

(define (add-activation-link g node1 node2)
  (add-edge g `((,node1 activation) (,node2 activation))))

(define (add-activation-links-to-ctx g new-nodes)
  (for*/fold ([g g])
             ([node new-nodes]
              [ctx (member-of g node)])
    (add-activation-link g node ctx)))

(define (make-map-of-value-instances g nodes)
  (for/fold ([hm (hash)])
            ([node nodes])
    (define v (get-node-attr g node 'value))
    (if (void? v)
      hm
      (hash-update hm v (λ (old) (cons node old)) '()))))

(define (merge-slipnet-graphs graphs)
  (for/fold ([sl (make-graph)] [hm-value->instances (hash)])
            ([g graphs])
    (let*-values ([(sl new-nodes) (copy-graph-into-graph sl g)]
                  [(sl) (add-activation-links-to-ctx sl new-nodes)]
                  [(hm) (make-map-of-value-instances sl new-nodes)])
      (values sl (hash-union hm-value->instances hm #:combine
                             (let () (local-require racket/base)
                               append))))))

(define (add-and-link-archetype-nodes sl hm-value->instances)
  (for/fold ([sl sl])
              ([hm-item (hash->list hm-value->instances)])
    (match-define `(,archetype-value . ,instances) hm-item)
    (let-values ([(sl archetype-node)
                      (make-node sl `((class . archetype)
                                      (value . ,archetype-value)))])
      (for/fold ([sl sl])
                ([instance instances])
        (add-activation-link sl archetype-node instance)))))

(define (has-top-level-slipnet-class? g node)
  (case (class-of g node)
    [(group archetype) #t]
    [else #f]))

(define (link-all-into-slipnet sl0)
  (let-values ([(sl topnode) (make-node sl0 '((class . slipnet)))])
    (for/fold ([sl sl])
              ([node (all-nodes sl0)]
               #:when (has-top-level-slipnet-class? sl0 node))
      (add-edge sl `((,topnode members) (,node member-of))))))

(define (make-slipnet . graphs)
  (let*-values ([(sl hm-value->instances) (merge-slipnet-graphs graphs)]
                [(sl) (add-and-link-archetype-nodes sl hm-value->instances)]
                [(sl) (link-all-into-slipnet sl)])
    sl))

;; Running the slipnet

(define slipnet-spreading-rate 0.1)
(define slipnet-decay 0.9)
(define slipnet-timesteps 10)

(define (activation-edges-starting-from g nodes)
  (for*/set ([node nodes]
             [edge (port->incident-edges g `(,node activation))])
    edge))

(define (add-activation activations node amount)
  (hash-update activations node (λ (old) (+ old amount)) 0.0))

(define (get-activation activations node)
  (dict-ref activations node 0.0))

(define (spread-activation-across-edge g initial-activations activations edge)
  (define (spread-1way as e)
    (match-define `((,from-node activation) (,to-node activation)) e)
    (add-activation as to-node
      (* slipnet-spreading-rate
         (get-activation initial-activations from-node))))
  (let* ([edge (set->list edge)]
         [activations (spread-1way activations edge)]
         [activations (spread-1way activations (reverse edge))])
    activations))

(define (decay-activations activations)
  (make-immutable-hash (for/list ([kv (hash->list activations)])
                         (match-define `(,node . ,a) kv)
                         `(,node . ,(* slipnet-decay a)))))

(define (do-slipnet-timestep g initial-activations)
  (for/fold ([activations (decay-activations initial-activations)])
            ([edge (activation-edges-starting-from g
                     (dict-keys initial-activations))])
    (spread-activation-across-edge g initial-activations activations edge)))

(define (run-slipnet g initial-activations)
  (for/fold ([activations initial-activations])
            ([timestep slipnet-timesteps])
    (do-slipnet-timestep g activations)))

(define (search-slipnet g initial-activations)
  'STUB)

(define (node->graph-delta g node)
  (match (class-of g node)
    ['bdx-scout
     (run-bdx-scout g node)]
    ['finish-archetype-instantiation
     (finish-archetype-instantiation g (port->neighbor g `(,node from-ctx))
                                       (port->neighbor g `(,node to-ctx)))]
    [_ #f]))

;NEXT Call this func and see if it works.
(define (do-timestep g)
  (define deltas (for/fold ([deltas '()])
                           ([node (all-nodes g)])
                   (let ([delta (node->graph-delta g node)])
                     (if delta (cons delta deltas) deltas))))
  #R deltas
  (do-graph-edits g deltas))

;(define g (make-graph 4))

;Need some sort ;of expr->graph func.
(define g (apply make-graph
  '((:group workspace
      4 5 6 15)
    (:group archetype
       4 5 + 9
       (:edge (4 result) (+ operands))
       (:edge (5 result) (+ operands))
       (:edge (+ result) (9 source)))
    (:make
      (:define finisher (:node finish-archetype-instantiation))
      (:edge (finisher from-ctx) (archetype general-port))
      (:edge (finisher to-ctx) (workspace general-port))))))


(define g1
  (make-graph '(:group 4+5=9 4 5 + 9
                 (:edge (4 result) (+ operands))
                 (:edge (5 result) (+ operands))
                 (:edge (+ result) (9 source)))))

(define g2
  (make-graph '(:group 4+2=6 4 2 + 6
                 (:edge (4 result) (+ operands))
                 (:edge (2 result) (+ operands))
                 (:edge (+ result) (6 source)))))

;(define-values (h new) (copy-graph-into-graph (make-graph) g1))
;(define-values (h new) (copy-graph-into-graph g1 g2))

;(pr-graph h)
;new
;(define hm (make-map-of-node-instances h new))
;hm

;(pr-graph g)

(define slipnet (make-slipnet
  (make-graph '(:group 4+5=9 4 5 + 9
                 (:edge (4 result) (+ operands))
                 (:edge (5 result) (+ operands))
                 (:edge (+ result) (9 source))))
  (make-graph '(:group 4+2=6 4 2 + 6
                 (:edge (4 result) (+ operands))
                 (:edge (2 result) (+ operands))
                 (:edge (+ result) (6 source))))
  (make-graph '(:group 6+9=15 6 9 + 15
                 (:edge (9 result) (+ operands))
                 (:edge (9 result) (+ operands))
                 (:edge (+ result) (15 source))))
  ))
                 
(pr-graph slipnet)

(define is #hash((archetype4 . 1.0)))
(define as (run-slipnet slipnet is))
as
