; make-slipnet.rkt --  Functions to make a slipnet, including archetype nodes
;                      and links for spreading activation

#lang debug at-exp racket

(require (prefix-in m: "model1.rkt")
         (prefix-in g: "graph1.rkt")
         (prefix-in f: "fargish1.rkt")
         (prefix-in sh: "shorthand.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require "wheel.rkt" predicates sugar)

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
         is-class)

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
      [(m:value-of-equal? g value (car atypes))
       (car atypes)]
      [else (loop (cdr atypes))])))

;HACK This isn't guaranteed to work, but it's way faster than the linear
;search in the commented-out function above.
;(define (archetype-of-value g value)
;  (define id 

(define (archetype-spec-of-node g node)
  (match (m:get-nodeclass-attr g node 'archetype-names)
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
      [(? is-value?) (f:archetype-name (m:value-of g node))]
      [(? is-class?) (f:archetype-name (m:class-of g node))]
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
     (make-archetype-for-value g (m:value-of g node))]
    [(? is-class?)
     (make-archetype-for-value g (m:class-of g node))]
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
  (let*-values ([(g archetype) (m:make-node/in g 'slipnet 'archetype value)]
                [(g) (g:set-node-attr g archetype 'value value)])
    (values g archetype)))

(define (link-in-new-archetype g archetype)
  (let-values ([(g slipnet) (find-or-make-slipnet g)])
    (g:add-edge g `((,slipnet archetypes) (,archetype slipnet)))))

(define (find-or-make-slipnet g)
  (cond
    [(g:has-node? g 'slipnet) (values g 'slipnet)]
    [else (m:make-node g 'slipnet)]))

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
    [(m:tag? sl new-node)
     (add-activation-edges sl new-node (m:taggees-of sl new-node) 1.0)]
    [(m:node-is-a? sl new-node 'ctx)
     (add-activation-edges sl new-node (m:members-of sl new-node) 0.1)]
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
    (for/fold ([sl (sh:make-graph spec '(slipnet))]
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

(define (sliplink-weight g from-node to-node [weight-if-no-edge 0.0])
  (g:graph-edge-weight g
                       `((,from-node activation) (,to-node activation))
                       weight-if-no-edge))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (require expect/rackunit (only-in rackunit test-case))
  (require "shorthand.rkt"
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass))

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
    (define tag (gdo m:make-node/in 'ws 'tag 22))
    (gdo make-archetype-for-node tag)
    (check-equal? (archetype-of-node g tag) 'archetype-tag-22))
  )
