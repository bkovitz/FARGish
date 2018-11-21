; sa.rkt -- Generic spreading activation
;
; Forward-only spreading

#lang debug at-exp racket

(require "wheel.rkt"
         (prefix-in g: "graph1.rkt"))
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile)

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
;(define (spread initial-activations front node->deltas)
;  (for/fold ([activations initial-activations]
;             [new-front empty-set])
;            ([(nodeid Δ) (make-deltas front node->deltas)])
;    (values (hash-update activations
;                         nodeid
;                         (λ (old-weight) (sat+ old-weight Δ))
;                         0.0)
;            (set-add new-front nodeid))))
;
;(define (make-deltas front node->deltas)
;  (for*/fold ([ht empty-hash])
;             ([from-node (in-set front)]
;              [delta (in-list (node->deltas from-node))])
;    (match-define `(,to-node . ,Δ) delta)
;    (hash-update ht to-node (λ (oldΔ) (+ Δ oldΔ)) 0.0)))

; initial-activations: (Hashof nodeid flonum?)
; node->hops nodeid -> (Hashof hop flonum?)
; prev-hops: #f or (Setof hop)
; On the first timestep, pass #f for prev-hops. On all following timesteps,
; pass the hops returned from the previous call.
; For simplicity, this function does no decay or saturation. 
; Returns two values: activations hops
(define (spread/forward initial-activations node->hops prev-hops)
  (for/fold ([activations initial-activations] [hops empty-set])
            ([(hop weight) (in-hash
                             (if prev-hops
                               (following-hops node->hops prev-hops)
                               (all-hops initial-activations node->hops)))])
    (values (apply-hop initial-activations activations hop weight)
            (set-add hops hop))))

(define (all-hops activations node->hops)
  (for*/fold ([ht empty-hash])
             ([from-node (in-list (hash-keys activations))]
              [(hop weight) (in-hash (node->hops from-node))])
    (hash-update ht hop (λ (oldΔ) (unsafe-fl+ weight oldΔ)) 0.0)))

(define (following-hops node->hops prev-hops)
  (for*/fold ([ht empty-hash])
             ([prev-hop (in-set prev-hops)]
              [(hop weight) (in-hash (node->hops (g:hop->to-node prev-hop)))]
              #:when (not (equal? hop (reverse prev-hop))))
    (hash-update ht hop (λ (oldΔ) (unsafe-fl+ weight oldΔ)) 0.0)))

; This attempt at optimization only made it slower
;(define (following-hops node->hops prev-hops)
;  (let prev-hops-loop ([ht empty-hash] [prev-hops (set->list prev-hops)])
;    (cond
;      [(null? prev-hops) ht]
;      [else (define prev-hop (car prev-hops))
;            (define reversed-prev-hop (reverse prev-hop))
;            (define ht-hops (node->hops (g:hop->to-node prev-hop)))
;            (let hops-loop ([ht ht]
;                            [pos (hash-iterate-first ht-hops)])
;              (if pos
;                (let-values ([(hop weight)
;                                (hash-iterate-key+value ht-hops pos)])
;                  (if (equal? hop reversed-prev-hop)
;                    (hops-loop ht (hash-iterate-next ht-hops pos))
;                    (hops-loop (hash-update ht
;                                            hop
;                                            (λ (oldδ) (unsafe-fl+ weight oldδ))
;                                            0.0)
;                               (hash-iterate-next ht-hops pos))))
;                (prev-hops-loop ht (cdr prev-hops))))])))

(define (activation-of activations node)
  (hash-ref activations node 0.0))

(define (apply-hop initial-activations activations hop weight)
  (define from-activation (activation-of initial-activations
                                         (g:hop->from-node hop)))
  (hash-update activations
               (g:hop->to-node hop)
               (λ (old-weight) (unsafe-fl+ old-weight (unsafe-fl* weight from-activation)))
               0.0))

;; ======================================================================
;;
;; unit test
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
      ;the edges are symmetric but activation should flow one-way, so
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

;; ----------------------------------------------------------------------

(module+ perftest
  (require (prefix-in g: "graph1.rkt")
           "shorthand.rkt"
           "model1.rkt"
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass))

  (random-seed 0)

  (define spec
    (farg-model-spec
      (nodeclass (number n)
        (value n)
        (name n))))

  (define num-nodes 100)
  (define num-edges 2000)
  (define g (let* ([g (make-graph spec)]
                   [g (for/fold ([g g])
                                ([n num-nodes])
                        (add-node g 'number n))]
                   [g (for/fold ([g g])
                                ([i num-edges])
                        (define node1 (random 0 num-nodes))
                        (define node2 (random 0 num-nodes))
                        (add-edge g `((,node1 activation) (,node2
                                                            activation))))])
               g))

  (define (node->hops from-node)
    (for/fold ([ht empty-hash])
              ([hop (in-list (g:port->incident-hops g `(,from-node activation)))])
      (hash-update ht
                   hop
                   (λ (oldΔ) (+ oldΔ (g:graph-edge-weight g hop)))
                   0.0)))

  (define initial-activations #hash((1 . 1.0) (2 . 1.0) (3 . 1.0) (4 . 1.0)))

  (define (spread initial-activations num-steps)
    (for/fold ([activations initial-activations] [prev-hops #f])
              ([t num-steps])
      (spread/forward activations node->hops prev-hops)))

  (define-values (as hops) (profile (spread initial-activations 10))))
