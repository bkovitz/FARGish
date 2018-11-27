; crawl.rkt -- Crawl a slipnet
;
; Crawling means searching via a narrowly focused form of spreading
; activation, where relatively few nodes are active at once, link
; weights are adjusted according to what you're searching for, and you
; stop when you find what you're searching for.

;TODO UT

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
(require plot pict pict/convert)
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile)

(provide make-crawler
         crawl)

(define spread-rate 0.01)
(define max-activation 2.0)

;; ======================================================================
;;
;; Crawler
;;

(struct crawler* (item-infos num-steps-taken) #:prefab)

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
                         (spread-activation-for-item-info g
                                                          archetype->edges
                                                          item-info))]
         [archetypes (top-archetypes (merge-activationss activationss))]
         [activationss (for/list ([activations activationss])
                         (pare-down-activations archetypes activations))]
         [item-infos (for/list ([item-info (crawler*-item-infos crawler)]
                                [activations activationss])
                       (struct-copy item-info* item-info
                                    [activations activations]))]
         [num-steps-taken (add1 (crawler*-num-steps-taken crawler))])
    (deposit-activations! (crawler* item-infos num-steps-taken))))

; TODO Rename this and/or provide a convenient interface to top-archetypes
(define (top-ars crawler [n 20])
  (define activationss (merge-activationss (activationss-of crawler)))
  (for/list ([a (sort (hash->list activationss) > #:key cdr)]
             [i n])
    (car a)))

(define (activationss-of crawler)
  (for/list ([item-info (crawler*-item-infos crawler)])
    (item-info*-activations item-info)))

(define (top-activations crawler [n 20])
  (define activationss (merge-activationss (activationss-of crawler)))
  (for/list ([a (sort (hash->list activationss) > #:key cdr)]
             [i n])
    a))

;; ======================================================================
;;
;; Ancillary functions to find edges and assign them weights
;;

; Returns (Listof (Setof nodeid)). These edges only list archetypes, unlike
; edges in graph1.rkt, which also include ports.
(define (make-archetype->edges g)
  (λ (archetype)
    (for/list ([hop (g:port->incident-hops g `(,archetype activation))])
      (match-define `((,k1 ,_) (,k2 ,_)) hop)
      (set k1 k2))))

; crude for now
(define (metric n m)
  (cond
    [(and (number? n) (number? m))
     (max 0.0 (- 1.0
                 (/ (abs (- n m))
                    10.0)))]
    [else 0.0]))

; Returns (g (Setof nodeid) -> flonum?) appropriate for item.
(define (make-edge->weight item)
  (match item
    [`(,class ,n)
      (λ (g edge)
        (match-define `(,archetype1 ,archetype2) (set->list edge))
        (define num-for-archetype1 (archetype->number g class n archetype1))
        (define num-for-archetype2 (archetype->number g class n archetype2))
        (cond
          [(void? num-for-archetype1) 1.0]
          [(void? num-for-archetype2) 1.0]
          [else (let* ([n1 (metric n num-for-archetype1)]
                       [n2 (metric n num-for-archetype2)]
                       [weight (- (* (+ n1 1.0)
                                     (+ n2 1.0))
                                  1.0)])
                  weight)]))]  ; weight will be in range 0.0 .. 3.0
    [else (λ (g edge) 1.0)]))

;TODO Something like this should be in the spec. That would enable inheritance
;to work.
(define (archetype->number g class target-number archetype)
  (define aclass (class-of g archetype))
  (cond
    [(eq? 'equation aclass) ;TODO inheritance
     (tags->number g class target-number archetype)]
    [(eq? 'archetype aclass) ;TODO inheritance
     (args->number class (args-of g archetype))]
    [else (void)]))

(define (args->number class args)
  (match (safe-car args)
    [`(,argclass ,n . ,_) #:when (and (equal? argclass class) ;TODO inheritance
                                      (number? n))
      n]
    [n #:when (number? n)
      n]
    [else (void)]))

(define (tags->number g tagclass target-number node)
  (safe-argmax (λ (tag-n)
                 (metric target-number tag-n))
               (for/list ([tag (tags-of g node)]
                          #:when (node-is-a? g tag tagclass))
                 (tagspec->number tagclass (tagspec-of g tag)))))

(define (tagspec->number class tagspec)
  (match tagspec
    [`(,tagclass ,n . ,_) #:when (equal? tagclass class) ;TODO inheritance
      n]
    [else (void)]))

(module+ test
  (require "numbo1.rkt")

  (test-case "edge->weight"
    (define edge->weight (make-edge->weight '(result 28)))
    (define g std-numbo-graph)
    (define w1 (edge->weight g (set 'archetype-result-28 'archetype-28)))
    (define w2 (edge->weight g (set 'archetype-result-28 '4*7=28)))
    (define w3 (edge->weight g (set 'archetype-has-operand-3 '3*8=24)))
    (define w4 (edge->weight g (set 'archetype-has-operand-3 '3*4=12)))
    (define w5 (edge->weight g (set 'archetype-24 '3*8=24))) ;inexact but good
    (define w6 (edge->weight g (set 'archetype-12 '3*4=12)))
    (check-equal? w1 3.0)
    (check-equal? w2 3.0)
    (check-equal? w3 1.0)
    (check-equal? w4 1.0)
    (check < w5 w1)
    (check < w3 w5)
    (check < w6 w5)
    (check-equal? w6 0.0)))

;; ======================================================================
;;
;; Ancillary functions for spreading activation
;;

(define (spread-activation-for-item-info g archetype->edges item-info)
  (spread-activation archetype->edges
                     (curry (item-info*-edge->weight item-info) g)
                     (item-info*-activations item-info)))

; archetype->edges: (nodeid -> (Listof (Setof nodeid)))
; edge->weight: (graph* (Setof nodeid) -> flonum?)
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

(define os (make-hash)) ; observations: key -> (t -> value)

(define (deposit! k t v)
  (hash-update! os
                k
                (λ (ht-t->v)
                     (hash-set! ht-t->v t v)
                     ht-t->v)
                (λ () (make-hash))))

(define (merged-activations crawler)
  (merge-activationss (for/list ([item-info (crawler*-item-infos crawler)])
                        (item-info*-activations item-info))))

(define (deposit-activations! crawler)
  (define t (crawler*-num-steps-taken crawler))
  (for ([(k v) (merged-activations crawler)])
    (deposit! k t v))
  crawler)

(define (timeseries k)
  (sorted-by-car
    (hash-ref os k empty-hash)))

; Plot activation history
(define (plot-ah . ks)
  (define hts (for/list ([k ks]) (hash-ref os k empty-hash)))
  (define tmin (apply min-key hts))
  (define tmax (apply max-key hts))
  (define ymin (safe-min 0.0 (apply min-value hts)))
  (define ymax (safe-max 1.0 (apply max-value hts)))
  (define (make-plot k)
    (define vs (reverse (for/list ([t-v (timeseries k)])
                          (list (car t-v) (cdr t-v)))))
    (plot-pict (lines vs) #:height 100
                          #:x-label #f #:y-label (~a k)
                          #:x-min tmin #:x-max tmax
                          #:y-min ymin #:y-max ymax))
  (apply vl-append (map make-plot ks)))

; ----------------------------------------------------------------------

(require "numbo1.rkt")

(define c (make-crawler g '((has-operand 3) (has-operand 8) (result 28))))

(define a1 (do-slipnet-timestep g (items->activations g '((has-operand 3)))))
(define a2 (do-slipnet-timestep g (items->activations g '((has-operand 8)))))
(define a3 (do-slipnet-timestep g (items->activations g '((result 28)))))

(define as (top-archetypes (merge-activationss (list a1 a2 a3))))

;(define d (time
;  (for/fold ([c c])
;            ([i 30])
;    (displayln @~a{timestep @i})
;    (define c* (crawl g c))
;    (pretty-print (top-activations c*))
;    (newline)
;    c*
;    )
;  ))

(define ii (third (crawler*-item-infos c)))
(define f (item-info*-edge->weight ii))
(f g (set 'archetype-result-25 '28-3=25))
