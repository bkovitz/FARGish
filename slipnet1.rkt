; slipnet1.rkt -- Code for making and querying a slipnet

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "model1.rkt" "shorthand.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require rackunit racket/pretty describe)

;; ======================================================================
;;
;; Archetypes
;; 

(define-singletons no-archetype is-node is-value is-class)

(define (archetypes g)
  (g:port->neighbors g '(slipnet archetypes)))

(define (node->archetype-type g node)
  (g:get-node-attr g node 'archetype-type no-archetype))

(define (archetype-of-value g value) ;TODO Extremely inefficient
  (let loop ([atypes (archetypes g)])
    (cond
      [(null? atypes)
       #f]
      [(value-of-equal? g value (car atypes))
       (car atypes)]
      [else (loop (cdr atypes))])))

(define (archetype-spec-of-node g node)
  (match (get-nodeclass-attr g node 'archetype-names)
    [`(,aname . ,_) aname]
    [else no-archetype]))  ; If no archetype-name defined, default is
                           ; no-archetype. Is this a good idea?

(define (archetype-of-node g node)
  (define archetype-nodeid
    (match (archetype-spec-of-node g node)
      [(? no-archetype?) (void)]
      [(? is-node?) node]
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
    (values g archetype-node)))

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
  (make-node/in g 'slipnet 'archetype value))

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
  (g:add-edge sl `((,from-node activation) (,to-node activation)) weight))

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

;TODO This is way too model- and tag-specific. Move to model1.rkt or tag1.rkt.
(define (taggees-of g tagnode)
  (g:port->neighbors g `(,tagnode tagged)))

(define (add-activation-edges-for sl new-node)
  (cond
    [(tag? sl new-node)
     (add-activation-edges sl new-node (taggees-of sl new-node) 0.2)]
    [(node-is-a? sl new-node 'ctx)
     (add-activation-edges sl new-node (members-of sl new-node) 0.1)]
    [else sl]))

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

  (test-case "make-slipnet"
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

    ;(pr-graph g)

    (check-equal? (list->set (archetypes g))
                  (list->set '(archetype-a archetype-b archetype-c)))
    ;TODO The groups should also be archetypes

    (check-true (g:has-edge? g `((archetype-a activation) (a-b activation))))
  )
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
    )
