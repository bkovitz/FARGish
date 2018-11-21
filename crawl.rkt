; crawl.rkt -- Crawl a slipnet
;
; Crawling means searching via a narrowly focused form of spreading
; activation, where relatively few nodes are active at once, link
; weights are adjusted according to what you're searching for, and you
; stop when you find what you're searching for.

#lang debug at-exp racket

(require "wheel.rkt"
         "model1.rkt"
         "slipnet1.rkt"
         (prefix-in f: "fargish1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile)

(provide make-crawler
         crawl)

(define spread-rate 0.01)
(define max-activation 2.0)

(struct crawler* (item-infos steps-taken) #:prefab)

(struct item-info* (item edge->weight activations) #:prefab)

; items = Listof items to search for, e.g. '((has-operand 3) (has-result 28)).
(define (make-crawler g items)
  (crawler* (map (λ (item) (make-item-info g item))
                 items)
            0))

(define (make-item-info g item)
  (item-info* item (make-edge->weight item) (item->activations g item)))

;TODO UT
(define (crawl g crawler)
  (let* ([archetype->edges (make-archetype->edges g)]
         [activationss (for/list ([item-info (crawler*-item-infos crawler)])
                         (spread-activation-for-item-info archetype->edges
                                                          item-info))]
         [archetypes (top-archetypes (merge-activationss activationss))]
         [activationss (for/list ([activations activationss])
                         (pare-down-activations archetypes activations))]
         [item-infos (for/list ([item-info (crawler*-item-infos crawler)]
                                [activations activationss])
                       (struct-copy item-info* item-info
                                    [activations activations]))]
         [steps-taken (add1 (crawler*-steps-taken crawler))])
    (crawler* item-infos steps-taken)))

(define (spread-activation-for-item-info archetype->edges item-info)
  (spread-activation archetype->edges
                     (item-info*-edge->weight item-info)
                     (item-info*-activations item-info)))

; Returns (Listof (Setof nodeid)). These edges only list archetypes, unlike
; edges in graph1.rkt, which also include ports.
(define (make-archetype->edges g)
  (λ (archetype)
    (for/list ([hop (g:port->incident-hops g `(,archetype activation))])
      (match-define `((,k1 ,_) (,k2 ,_)) hop)
      (set k1 k2))))

(define (make-edge->weight items)
  (λ (edge)  ; edge: (Setof nodeid)
    1.0)) ;STUB

; archetype->edges: (nodeid -> (Listof (Setof nodeid)))
; edge->weight: ((Setof nodeid) -> flonum?)
(define (spread-activation archetype->edges edge->weight initial-activations)
  (define (spread-activation-across-edge activations edge)
    (match-define `(,k1 ,k2) (set->list edge))
    (define weight (edge->weight edge))
    (let* ([activations (add-activation activations k1 k2 weight)]
           [activations (add-activation activations k2 k1 weight)])
      activations))

  (define (add-activation activations from to edge-weight)
    (define delta (* spread-rate
                     edge-weight
                     (hash-ref initial-activations from 0.0)))
    (hash-update activations
                 to
                 (λ (old) (unsafe-sat+ old delta))
                 0.0))

  (define edges (for*/fold ([st empty-set])
                           ([archetype (hash-keys initial-activations)]
                            [e (archetype->edges archetype)])
                  (set-add st e)))

  (for*/fold ([activations initial-activations])
             ([edge edges])
    (spread-activation-across-edge activations edge)))

(define (items->activations g items)
  (for/fold ([ht empty-hash])
            ([item items])
    (cond
      [(is-archetype? g item)
       (hash-update ht item (curry + 1.0) 0.0)]
      [else
       (hash-update ht (f:archetype-name item) (curry + 1.0) 0.0)])))

(define (item->activations g item)
  (cond
    [(is-archetype? g item)
     (hash item 1.0)]
    [else
     (hash (f:archetype-name item) 1.0)]))

(define (merge-activationss activationss)
  (apply hash-union activationss #:combine +))

(define (top-archetypes activations [n 100])
  (for/set ([a (sort (hash->list activations) > #:key cdr)]
            [i n])
    (car a)))

(define (pare-down-activations archetypes-to-retain activations)
  (for/hash ([(k v) activations]
             #:when (set-member? archetypes-to-retain k))
    (values k v)))

(define (unsafe-sat+ . args)
  (min (apply unsafe-fl+ args)
       max-activation))

; ----------------------------------------------------------------------

(require "numbo1.rkt")

(define c (make-crawler g '((has-operand 3) (has-operand 8) (result 28))))

(define a1 (do-slipnet-timestep g (items->activations g '((has-operand 3)))))
(define a2 (do-slipnet-timestep g (items->activations g '((has-operand 8)))))
(define a3 (do-slipnet-timestep g (items->activations g '((result 28)))))

(define as (top-archetypes (merge-activationss (list a1 a2 a3))))

(define d (time (for/fold ([c c])
                    ([i 10])
            (crawl g c))))
