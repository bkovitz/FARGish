; slipnet1.rkt -- Code for making and querying a slipnet

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         "model1.rkt"
         "shorthand.rkt"
         ;"sa.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe)

(provide archetypes
         archetype-of-value
         archetype-of-node
         add-group-to-slipnet
         link-archetypally
         group-members-and-tags
         is-archetype?
         sliplink-weight

         no-archetype
         is-node
         is-value
         is-class

         run-slipnet
         do-slipnet-timestep
         decay-activations
         ss)

(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 10)

;; ======================================================================
;;
;; Archetypes
;; 

(define-singletons no-archetype is-node is-value is-class)

(define (archetypes g)
  (g:port->neighbors g '(slipnet archetypes)))

(define (is-archetype? g id)
  (member id (archetypes g)))

(define (node->archetype-type g node)
  (g:get-node-attr g node 'archetype-type no-archetype))

(define (archetype-of-value g value) ;TODO Extremely inefficient
  (let loop ([atypes (archetypes g)])
    (cond
      [(null? atypes)
       (void)]
      [(value-of-equal? g value (car atypes))
       (car atypes)]
      [else (loop (cdr atypes))])))

;HACK This isn't guaranteed to work, but it's way faster than the linear
;search in the commented-out function above.
;(define (archetype-of-value g value)
;  (define id 

(define (archetype-spec-of-node g node)
  (match (get-nodeclass-attr g node 'archetype-names)
    [`(,aname . ,_) aname]
    [else no-archetype]))  ; If no archetype-name defined, default is
                           ; no-archetype. Is this a good idea?

(define (archetype-of-node g node)
  (define archetype-nodeid
    (match (archetype-spec-of-node g node)
      [(? no-archetype?) (void)]
      [(? is-node?)
       (if (g:has-edge? g `((slipnet archetypes) (,node slipnet)))
         node
         (void))]
      [(? is-value?) (f:archetype-name (value-of g node))]
      [(? is-class?) (f:archetype-name (class-of g node))]
      [v (f:archetype-name v)]))
  (cond
    [(void? archetype-nodeid) (void)]
    [(g:has-node? g archetype-nodeid) archetype-nodeid]
    [else (void)]))

; Makes archetype for node if one does not already exist. Links it to
; 'slipnet node if it's not already linked. Returns two values: g archetype.
; If the node does not get an archetype, returns g <void>.
(define (get-or-make-archetype-for-node g node)
  (define archetype-node (archetype-of-node g node))
  (if (void? archetype-node)
    (make-archetype-for-node g node)
    (values (ensure-member-of-slipnet g archetype-node) archetype-node)))

; Makes archetype for value if one does not already exist. Links it to
; 'slipnet node if it's not already linked. Returns two values: g archetype.
(define (get-or-make-archetype-for-value g value)
  (define archetype-node (archetype-of-value g value))
  (if (void? archetype-node)
    (make-archetype-for-value g value)
    (values (ensure-member-of-slipnet g archetype-node) archetype-node)))

(define (ensure-member-of-slipnet g node)
  (g:add-edge g `((slipnet members) (,node member-of))))

(define (make-archetype-for-node g node)
  (match (archetype-spec-of-node g node)
    [(? void?)
     (values g (void))]
    [(? no-archetype?)
     (values g (void))]
    [(? is-value?)
     (make-archetype-for-value g (value-of g node))]
    [(? is-class?)
     (make-archetype-for-value g (class-of g node))]
    [(? is-node?)
     (values (g:add-edge g `((slipnet archetypes) (,node slipnet))) node)]
    [v
     (make-archetype-for-value g v)]))



;  (define (get-or-make avalue)
;    (cond
;      [(archetype-of-value g avalue)
;       => (λ (found-archetype) (values g found-archetype))]
;      [else (make-archetype-for-value g avalue)]))
;  (match (node->archetype-type g node)
;    [(? no-archetype?) (values g (void))]
;    [(? is-node?)
;     (values (link-in-new-archetype g node) node)]
;    [(? is-value?)
;     (get-or-make (value-of g node))]
;    [(? is-class?)
;     (get-or-make (class-of g node))]))

;Then apply it to archetype.
;HACK for now: name it here.

;(define (make-archetype-for-value g value)
;  (let*-values ([(g archetype) (make-node-with-attrs g
;                                 (hash 'class 'archetype
;                                       'value value
;                                       'name (archetype-name value)))]
;                [(g) (link-in-new-archetype g archetype)])
;    (values g archetype)))
(define (make-archetype-for-value g value)
  (let*-values ([(g archetype) (make-node/in g 'slipnet 'archetype value)]
                [(g) (g:set-node-attr g archetype 'value value)])
    (values g archetype)))

(define (link-in-new-archetype g archetype)
  (let-values ([(g slipnet) (find-or-make-slipnet g)])
    (g:add-edge g `((,slipnet archetypes) (,archetype slipnet)))))

(define (find-or-make-slipnet g)
  (cond
    [(g:has-node? g 'slipnet) (values g 'slipnet)]
    [else (make-node g 'slipnet)]))

;; ======================================================================
;;
;; Making a slipnet
;; 

(define (add-activation-edge sl from-node to-node [weight 1.0])
  (let* ([edge `((,from-node activation) (,to-node activation))]
         [old-weight (g:graph-edge-weight sl edge)]
         [weight (if (void? old-weight)
                   weight
                   (+ weight old-weight))])
    (g:add-edge sl edge weight)))

(define (has-activation-edge? sl node1 node2)
  (g:has-edge? sl `((,node1 activation) (,node2 activation))))

(define (add-activation-edges sl from-node to-nodes [weight 1.0])
  (define from-archetype (archetype-of-node sl from-node))
  (if (void? from-archetype)
    sl
    (for/fold ([sl sl])
              ([to-node to-nodes])
      (define to-archetype (archetype-of-node sl to-node))
      (if (void? to-archetype)
        sl
        (add-activation-edge sl from-archetype to-archetype weight)))))
    ;(add-edge sl `((,from-archetype activation) (,to-archetype activation)))))

(define (add-activation-edges-for sl new-node)
  (cond
    [(tag? sl new-node)
     (add-activation-edges sl new-node (taggees-of sl new-node) 1.0)]
    [(node-is-a? sl new-node 'ctx)
     (add-activation-edges sl new-node (members-of sl new-node) 0.1)]
    [else sl]))

;TODO Get rid of g
(define (add-archetypes-for-new-nodes slipnet g new-nodes)
  (for/fold ([sl slipnet])
            ([new-node new-nodes])
    (let-values ([(sl atype) (get-or-make-archetype-for-node sl new-node)])
      sl)))

(define (add-activation-edges-for-new-nodes sl new-nodes)
  (for/fold ([sl sl])
            ([new-node new-nodes])
    (add-activation-edges-for sl new-node)))

(define (make-slipnet spec . graphs)
  (define-values (sl new-nodes)
    (for/fold ([sl (make-graph spec '(slipnet))]
               [new-nodes (set)])
              ([g graphs])
      (let*-values ([(sl node-map) (g:copy-graph-into-graph sl g)]
                    [(news) (apply set (hash-values node-map))]
                    [(sl) (add-archetypes-for-new-nodes sl g news)]
                    [(new-nodes) (set-union new-nodes news)])
        (values sl new-nodes))))
  (add-activation-edges-for-new-nodes sl new-nodes))

; group is a nodeid. Its tags and members will get archetypes and activation
; links.
(define (add-group-to-slipnet g group)
  (let* ([nodes (group-members-and-tags g group)]
         [g (add-archetypes-for-new-nodes g 'ignored nodes)]
         [g (add-activation-edges-for-new-nodes g nodes)])
    g))

(define (group-members-and-tags g group)
  (set-union (set group)
             (list->set (g:members-of g group))
             (list->set (g:port->neighbors g `(,group tags))))) ;HACK

(define (link-archetypally g archetype value [weight 1.0])
  (let*-values ([(g value-archetype) (get-or-make-archetype-for-value g value)]
                [(g) (add-activation-edge g archetype value-archetype weight)])
    g))

;; ======================================================================
;;
;; Spreading activation
;;

(define (run-slipnet g initial-activations)
  (for/fold ([activations initial-activations])
            ([timestep slipnet-timesteps])
    (let ([as (maybe-suspend 'slipnet-activations
                             (do-slipnet-timestep g activations))])
      ;#R (take-right (sorted-by-cdr as) 20)
      as)))

(define (do-slipnet-timestep g initial-activations)
  (for/fold ([activations (decay-activations initial-activations)])
            ([edge (activation-edges-starting-from g
                     (hash-keys initial-activations))])
    (spread-activation-across-edge g initial-activations activations edge)))

(define (decay-activations activations [decay-rate slipnet-decay])
  (make-immutable-hash (for/list ([(node a) (in-hash activations)])
                         `(,node . ,(* decay-rate a)))))

(define (activation-edges-starting-from g nodes)
  (for*/set ([node nodes]
             [edge (g:port->incident-edges g `(,node activation))])
    edge))

(define (spread-activation-across-edge g initial-activations activations edge)
  (define weight (match-let ([`((,from ,_) (,to ,_)) (set->list edge)])
                   (sliplink-weight g from to)))
  (define (spread-1way activations hop)
    (match-define `((,from-node ,_) (,to-node ,_)) hop) ;TODO catch self-link?
    (define r (* 0.2 (- (random) 0.5) weight))
    (add-activation activations to-node
      (* slipnet-spreading-rate
         (+ weight r)
         (get-activation initial-activations from-node))))
  (let* ([hop (set->list edge)]
         [activations (spread-1way activations hop)]
         [activations (spread-1way activations (reverse hop))])
    activations))

(define (sliplink-weight g from-node to-node [weight-if-no-edge 0.0])
  (g:graph-edge-weight g
                       `((,from-node activation) (,to-node activation))
                       weight-if-no-edge))

(define (add-activation activations node amount)
  (hash-update activations
               node
               (λ (old) (min 2.0 (+ old amount)))
               0.0))

(define (get-activation activations node)
  (hash-ref activations node 0.0))

;; ======================================================================
;;
;; Searching a slipnet
;;

(define (ss g . items)
  (define initial-activations
    (for/fold ([ht empty-hash])
              ([item items])
      (cond
        [(list? item)
         (hash-update ht (f:archetype-name item)
           (curry + 1.0) 0.0)]
        [else
         (let ([item (if (is-archetype? g item)
                       item
                       (f:archetype-name item))])
           (hash-update ht item (curry + 1.0) 0.0))])))
  (sorted-by-cdr (run-forward-slipnet g initial-activations))
  #;(take-right (sorted-by-cdr (run-forward-slipnet g initial-activations)) 20))

(define (run-forward-slipnet g initial-activations)
  (define n->h (curry node->hops g))
  (for/fold ([activations initial-activations] [prev-hops #f]
             #:result activations)
            ([t slipnet-timesteps])
    (let* ([activations (if prev-hops
                          (decay-activations activations)
                          activations)])
      (let ([len (length (hash-keys activations))])
        #R t
        #R len
        #R (take-right (sorted-by-cdr activations) (min 10 len))
        (void))
      (spread/forward activations n->h prev-hops))))

(define (node->hops g from-node)
  (for/fold ([ht empty-hash])
            ([hop (in-list (g:port->incident-hops g `(,from-node activation)))])
    (hash-update ht
                 hop
                 (λ (oldΔ) (fl+ oldΔ
                                (fl* slipnet-spreading-rate
                                     (g:graph-edge-weight g hop))))
                 0.0)))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
;  (define spec0
;    (farg-model-spec
;      (nodeclass ws
;        (is-a 'ctx)) ; doesn't have an archetype
;      (nodeclass number
;        (is-a 'ctx)
;        (archetype is-value))
;      (nodeclass operator
;        (archetype is-class))
;      (nodeclass +
;        (is-a 'operator))
;      (nodeclass equation
;        (is-a 'ctx)
;        (archetype is-node))))

  (define spec
    (farg-model-spec
      (nodeclass (letter a)
        (name a)
        (value a)
        (archetype a))
      (nodeclass (group nm)
        (is-a 'ctx)
        (name nm)
        (archetype is-node))))

  (define group1
    (make-graph spec
      '(:in (group a-b)
         (:edge (letter a) out (letter b) in))))

  (define group2
    (make-graph spec
      '(:in (group a-c)
         (:edge (letter a) out (letter c) in))))

  (test-case "make-slipnet"
    (define g (make-slipnet spec group1 group2)) 

    (check-equal? (list->set (archetypes g))
                  (list->set '(a-b a-c archetype-a archetype-b archetype-c)))

    (check-true (g:has-edge? g `((archetype-a activation) (a-b activation))))
  )

  (test-case "spreading activation in slipnet"
    (define spec
      (farg-model-spec
        (nodeclass (letter a)
          (name a)
          (value a)
          (archetype a))
        (nodeclass (group nm)
          (is-a 'ctx)
          (name nm)
          (archetype is-node))))

    (define group1
      (make-graph spec
        '(:in (group a-b)
           (:edge (letter a) out (letter b) in))))

    (define group2
      (make-graph spec
        '(:in (group a-c)
           (:edge (letter a) out (letter c) in))))

    (define g (make-slipnet spec group1 group2)) 

    (define initial-activations (hash 'archetype-a 1.0))

    (define activations (run-slipnet g initial-activations))

    (check-true (> (hash-ref activations 'a-b) 0.0))
    (check-true (> (hash-ref activations 'a-c) 0.0))
    (check-true (> (hash-ref activations 'archetype-b) 0.0))
    (check-true (> (hash-ref activations 'archetype-c) 0.0))

    (check-true (> (hash-ref activations 'archetype-a)
                   (hash-ref activations 'a-b)
                   (hash-ref activations 'archetype-b)))
    (check-true (> (hash-ref activations 'archetype-a)
                   (hash-ref activations 'a-c)
                   (hash-ref activations 'archetype-c))))

    
;    (define group1
;      (make-graph spec
;        '(:in (group a-b)
;           (:edge (letter a) out (letter b) in))))
;
;    (define slipnet (make-slipnet
;      (make-equation-graph 4 '+ 5 9)
;      (make-equation-graph 4 '+ 2 6)
;      (make-equation-graph 6 '+ 9 15)))
;    (check-not-false (exactly-one? (curry value-of-equal? slipnet 4)
;                                   (archetypes slipnet)))
;    ;There should also be archetypes for the various tags provided by
;    ;make-equation-graph.
;    (define atype4 (archetype-of-value slipnet 4))
;    (check-false (void? atype4))
;;    (pr-graph slipnet) ;DEBUG
;;    (newline)
;;    (println (port->neighbors slipnet '(slipnet archetypes)))
;;    (newline)
;;    (println (all-nodes slipnet))
  
  (test-case "default archetype name for tag"
    (define spec
      (farg-model-spec
        (nodeclass (letter a)
          (name a)
          (value a))
        (tagclass (tag n)
          (applies-to ([node])
            (condition (const #t))))))

    (define g (make-slipnet spec))
    (define tag (gdo make-node/in 'ws 'tag 22))
    (gdo make-archetype-for-node tag)
    (check-equal? (archetype-of-node g tag) 'archetype-tag-22))
)


;(gdo do-slipnet-timestep initial-activations)