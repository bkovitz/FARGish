; sa.rkt -- Generic spreading activation
;
; Forward-only spreading

#lang debug at-exp errortrace racket

(require "wheel.rkt")
(require rackunit racket/pretty describe)

(provide spread/forward)

;(define decay-rate 0.9)
;(define spread-rate 0.1)
;(define max-activation 3.0)
;
;(define (decay activations)
;  (hash-map-values activations (λ (a) (* decay-rate a))))

;(define (sat+ . args)
;  (min (apply + args)
;       max-activation))

; FAILED ATTEMPT at forward-only spreading. Could be the basis of
; spread/bidirectional.
; initial-activations: (Hashof nodeid flonum?)
; front: (Setof nodeid)
; node->deltas: nodeid -> (Listof (Cons nodeid delta))
; Returns two values: activations new-front
(define (spread initial-activations front node->deltas)
  (for/fold ([activations initial-activations]
             [new-front empty-set])
            ([(nodeid Δ) (make-deltas front node->deltas)])
    (values (hash-update activations
                         nodeid
                         (λ (old-weight) (sat+ old-weight Δ))
                         0.0)
            (set-add new-front nodeid))))

(define (make-deltas front node->deltas)
  (for*/fold ([ht empty-hash])
             ([from-node (in-set front)]
              [delta (in-list (node->deltas from-node))])
    (match-define `(,to-node . ,Δ) delta)
    (hash-update ht to-node (λ (oldΔ) (+ Δ oldΔ)) 0.0)))

; initial-activations: (Hashof nodeid flonum?)
; node->hops nodeid -> (Hashof hop flonum?)
; prev-hops: #f or (Setof hop)
; On the first timestep, pass #f for prev-hops. On all following timesteps,
; pass the hops returned from the previous call.
; For simplicity, this function does no decay or saturation. 
; Returns two values: activations hops
(define (spread/forward initial-activations node->hops prev-hops)
  (for/fold ([activations initial-activations] [hops empty-set])
            ([(hop weight) (if prev-hops
                             (following-hops node->hops prev-hops)
                             (all-hops initial-activations node->hops))])
    (values (apply-hop initial-activations activations hop weight)
            (set-add hops hop))))

(define (all-hops activations node->hops)
  (for*/fold ([ht empty-hash])
             ([from-node (hash-keys activations)]
              [(hop weight) (node->hops from-node)])
    (hash-update ht hop (λ (oldΔ) (+ weight oldΔ)) 0.0)))

(define (following-hops node->hops prev-hops)
  (for*/fold ([ht empty-hash])
             ([prev-hop prev-hops]
              [(hop weight) (node->hops (hop->to-node prev-hop))]
              #:when (not (equal? hop (reverse prev-hop))))
    (hash-update ht hop (λ (oldΔ) (+ weight oldΔ)) 0.0)))

(define (activation-of activations node)
  (hash-ref activations node 0.0))

(define (apply-hop initial-activations activations hop weight)
  (define from-activation (activation-of initial-activations
                                         (hop->from-node hop)))
  (hash-update activations
               (hop->to-node hop)
               (λ (old-weight) (+ old-weight (* weight from-activation)))
               0.0))

(define (hop->to-node hop)
  (match-define `(,from-port (,to-node ,_)) hop)
  to-node)

(define (hop->from-node hop)
  (match-define `((,from-node ,_) ,to-port) hop)
  from-node)

;; ======================================================================
;;
;; Unit test
;;

(module+ test
  (require (prefix-in g: "graph1.rkt")
           "shorthand.rkt"
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass by-ports as-member))

  (define spec
    (farg-model-spec
      (nodeclass (letter a)
        (name a)
        (value a))))

  (test-case "spread/forward"
    (define initial-activations #hash((a . 1.0)))
    (define g (make-graph spec '(:let ([a (letter a)]
                                       [b (letter b)]
                                       [c (letter c)]
                                       [d (letter d)])
                                  (:edge a activation b activation 0.1)
                                  (:edge a activation c activation 0.2)
                                  (:edge a activation d activation 0.3)
                                  (:edge b activation c activation 0.1))))
      ;The edges are symmetric but activation should flow one-way, so
      ; a -> b on the first timestep, b -> c but not b -> a on the second
      ;timestep.
    (define (node->hops from-node)
      (for/fold ([ht empty-hash])
                ([hop (g:port->incident-hops g `(,from-node activation))])
        (hash-update ht
                     hop
                     (λ (oldΔ) (+ oldΔ (g:graph-edge-weight g hop)))
                     0.0)))

    (define (hop node1 node2)
      `((,node1 activation) (,node2 activation)))

    (let*-values ([(activations prev-hops)
                     (spread/forward initial-activations node->hops #f)]
                                            ; first step: prev-hops = #f
                  [(_) (check-equal? prev-hops
                                     (set (hop 'a 'b)
                                          (hop 'a 'c)
                                          (hop 'a 'd)))]
                  [(_) (check-equal? (trunc-all activations)
                                     (hash 'a 1.0
                                           'b 0.1
                                           'c 0.2
                                           'd 0.3))]
                  [(_) (check-equal? (list->set
                                       (hash-keys
                                         (following-hops node->hops prev-hops)))
                                     (set (hop 'b 'c)
                                          (hop 'c 'b)))]
                  [(activations prev-hops)
                     (spread/forward activations node->hops prev-hops)]
                  [(_) (check-equal? (trunc-all activations)
                                     (hash 'a 1.0
                                           'b 0.12
                                           'c 0.21
                                           'd 0.3))]
                  [(_) (check-equal? (list->set
                                       (hash-keys
                                         (following-hops node->hops prev-hops)))
                                     (set (hop 'b 'a)
                                          (hop 'c 'a)))]
                  [(activations prev-hops)
                     (spread/forward activations node->hops prev-hops)]
                  [(_) (check-equal? (trunc-all activations)
                                     (hash 'a 1.054
                                           'b 0.12
                                           'c 0.21
                                           'd 0.3))])
      (void))))
