; spreading-activation.rkt -- Generic spreading activation

; There are other reasonable ways to define spreading activation than what
; happens in spread-activation, but at least it's defined independently of
; everything else.

#lang debug at-exp racket

(require "wheel.rkt")
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile)

(provide spread-activation
         default-spreading-activation-params
         set-weight-noisef)

;(struct spreading-activation-params*
;        (decay-rate spread-rate max-activation weight-noisef) #:transparent)
; decay-rate : flonum?
;   Each node's activation is multiplied by decay-rate at start of each
;   timestep.
; spread-rate : flonum?
;   Activation sent from node through a hop is multiplied by spread-rate.
; max-activation : flonum?
;   No node's activation may exceed max-activation.
; weight-noisef : edge-weight -> edge-weight
;   Function called to add randomness to edge-weights.

(module t typed/racket
  (require racket/flonum racket/unsafe/ops)
  (provide spread-activation
           default-spreading-activation-params
           set-weight-noisef
           (struct-out spreading-activation-params*))

  (define-type Node (U Symbol Integer))
  (define-type Edge (Setof Node))  ; 2 elements
  (define-type ATable (Immutable-HashTable Node Flonum)) ; activations table

  ;(define empty-set : (Setof Any) (set))

  (: hash-map-values (All (K V) (-> (HashTable K V)
                                    (-> V V)
                                    (Immutable-HashTable K V))))
  (define (hash-map-values ht f)
    (for/hash : (Immutable-HashTable K V) ([(k v) ht])
      (values k (f v))))


  (struct spreading-activation-params*
          ([decay-rate : Flonum]
           [spread-rate : Flonum]
           [max-activation : Flonum]
           [weight-noisef : (Flonum -> Flonum)])
          #:transparent)

  (define default-spreading-activation-params
    (spreading-activation-params*
      0.9
      0.1
      10.0
      (λ (w) (+ w (* 0.2 (- (random) 0.5))))))

  (: set-weight-noisef : spreading-activation-params* (Flonum -> Flonum)
                         -> spreading-activation-params*)
  (define (set-weight-noisef params f)
    (struct-copy spreading-activation-params* params
      [weight-noisef f]))

; sa-params : spreading-activation-params*
; initial-activations : (HashTable node flonum?)
; node->neighbors : node -> (list node)
;   Function that returns all of a node's neighbors that activation should
;   spread to.
; edge->weight : (Setof2 node) -> flonum?
;   Function that returns the weight of edge between two nodes, for purposes
;   of spreading activation.
(: spread-activation : spreading-activation-params*
                       ATable
                       (-> Node (Listof Node))
                       (-> Edge Flonum)
                       -> ATable)
(define (spread-activation sa-params
                           initial-activations
                           node->neighbors
                           edge->weight)
  (define decay-rate (spreading-activation-params*-decay-rate sa-params))
  (define spread-rate (spreading-activation-params*-spread-rate sa-params))
  (define max-activation
    (spreading-activation-params*-max-activation sa-params))
  (define weight-noisef (spreading-activation-params*-weight-noisef sa-params))

  (: spread-activation-across-edge : ATable Edge -> ATable)
  (define (spread-activation-across-edge activations edge)
    (match-define `(,k1 ,k2) (set->list edge))
    (define weight (edge->weight edge))
    (let* ([activations (add-activation activations k1 k2 weight)]
           [activations (add-activation activations k2 k1 weight)])
      activations))

  (: add-activation : ATable Node Node Flonum -> ATable)
  (define (add-activation activations from-node to-node edge-weight)
    (define delta (weight-noisef
                    (* spread-rate
                       (hash-ref initial-activations from-node (const 0.0))
                       edge-weight)))
    (hash-update activations
                 to-node
                 (λ ([old : Flonum])
                   (min max-activation
                        (fl+ old delta)))
                 (const 0.0)))

  (define edges (for*/fold ([st : (Setof Edge) (set)])
                           ([node (hash-keys initial-activations)]
                            [neighbor (node->neighbors node)])
                  (set-add st (set node neighbor))))

  (define (decay [x : Flonum]) (* x decay-rate))
  (for/fold ([activations : ATable (hash-map-values initial-activations decay)])
            ([edge edges])
    (spread-activation-across-edge activations edge)))

  )

(require 't)

; Converting the unit test to Typed Racket looks like it will require a lot
; of require/typed work
;(module ttest typed/racket
;  (require/typed "fargish1.rkt"
;                 [#:struct farg-model-spec* 
;                   ([nodeclasses : (Hashof Symbol nodeclass*)
;
;    (define spec
;      (farg-model-spec
;        (nodeclass (letter a)
;          (value a)
;          (name a))))
;)

(module+ test
  (require (prefix-in g: "graph1.rkt")
           "fargish1.rkt"
           "shorthand.rkt")

  (test-case "spread-activation test"
    (define spec
      (farg-model-spec
        (nodeclass (letter a)
          (value a)
          (name a))))
    (define g (make-graph spec '(:let ([a (letter a)]
                                       [b (letter b)]
                                       [c (letter c)]
                                       [d (letter d)])
                                  (:edge a sa-port b sa-port 1.0)
                                  (:edge a sa-port c sa-port 2.0)
                                  (:edge a ignored d ignored))))
    (define (node->port node)
      `(,node sa-port))
    (define (2nodes->edge 2nodes)
      (map node->port (set->list 2nodes)))

    ;TODO Call set-weight-noisef
    (define sa-params (struct-copy spreading-activation-params*
                                   default-spreading-activation-params
                        [weight-noisef identity])) ;disable randomness
    (define (spread activations)
       (spread-activation sa-params activations
         (λ (node) (g:port->neighbors g (node->port node)))
         (λ (2nodes) (g:graph-edge-weight g (2nodes->edge 2nodes)))))

    (define initial-activations (hash 'a 1.0))
    (let* ([activations (spread initial-activations)]
           [_ (check-equal? activations (hash 'a 0.9
                                              'b 0.1
                                              'c 0.2))]
           [activations (spread activations)]
           [_ (check-equal? (round-all/ut activations)
                            (hash 'a 0.86
                                  'b 0.18
                                  'c 0.36))])
      (void))))
