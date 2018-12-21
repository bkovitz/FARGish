; ABANDONED -- This version is an interpreter for lists that represent
; search-items.

; SECOND VERSION -- IN PROGRESS
; Moving stuff out to search-item.rkt and sa.rkt

; crawl.rkt -- Crawl a slipnet
;
; Crawling means searching via a narrowly focused form of spreading
; activation, where relatively few nodes are active at once, link
; weights are adjusted according to what you're searching for, and you
; stop when you find what you're searching for.

;TODO UT

#lang debug at-exp racket

(require "wheel.rkt"
         "observe.rkt"
         "model1.rkt"
         "slipnet1.rkt"
         "search-item.rkt"
         (prefix-in f: "fargish1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require data/gvector plot pict pict/convert)
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile racket/enter racket/trace)

(provide make-crawler
         crawl-to-completion
         crawl-step
         crawler-found)

(define spread-rate 0.01)
(define max-activation 10.0)
(define spread-decay 0.9)
(define num-top-archetypes 1000)
(define done-threshold 0.01)


'(crawler
   (seek-instance-of equation
     (require (tagged-with? (has-result
                              (exact-number (node-available-as target)))))
     (prefer (tagged-with? (has-operand
                              (inexact-number (node-available-as brick)))))))

'(crawler
   (in-ws ([t (node-available-as target)]
           [b (node-available-as brick)])
     (seek-instance-of equation
       (require (tagged-with? (has-result (exact-number t))))
       (prefer (tagged-with? (has-operand (inexact-number b)))))))

'(crawler
   (in-ws ([t (node-available-as target)]
           [b (node-available-as brick)])
     (seek-instance-of equation
       (require (has-result (exact-number t)))
       (prefer (has-operand (inexact-number b))))))

'(crawler
   (seek-instance-of equation
     (require (tagged-with? (has-operand (exact brick)
                                         (count (at-least 2)))
                            (has-result (inexact target))))))

(struct require-archetype* (archetype) #:prefab)
(struct require-tagged* (class n) #:prefab)
(struct prefer-tagged* (class n) #:prefab)
(struct exact-number* (n) #:prefab)
(struct inexact-number* (n) #:prefab)

(define (make-required preference)
  (require-tagged* (prefer-tagged*-class preference)
                   (prefer-tagged*-n preference)))

                              ; variable -> nodespec
(define (make-crawler crawler-spec)
  (define (parse-crawler-spec crawler-spec ws-nodespecs search-item)
    (match crawler-spec
      ['() (values ws-nodespecs search-item)]
      [`(crawler . ,more)
       (parse-crawler-spec more ws-nodespecs search-item)]
      [`((in-ws ,ws-items . ,body) . ,more)
        (let*-values ([(ws-nodespecs) (parse-ws-items ws-nodespecs ws-items)]
                      [(ws-nodespecs search-item)
                         (parse-crawler-spec body ws-nodespecs search-item)])
          (parse-crawler-spec more ws-nodespecs search-item))]
      [`((seek-instance-of ,archetype . ,search-items) . ,more)
        (let ([new-search-item (parse-search-items
                                 (list (require-archetype* archetype))
                                 search-items)])
          (parse-crawler-spec
              more ws-nodespecs (cons new-search-item search-item)))]
      [else (raise-arguments-error 'make-crawler
              @~a{Invalid crawler-spec item: @crawler-spec})]))

  (define (parse-ws-items ws-nodespecs ws-items)
    (match ws-items
      ['() ws-nodespecs]
      [`([,name ,tagspec] . ,more)
        (parse-ws-items (hash-set ws-nodespecs name tagspec) more)]
      [else (raise-arguments-error 'make-crawler
              @~a{Invalid workspace item: @ws-items})]))

  (define (parse-search-items items-so-far search-items)
    (match search-items
      ['() items-so-far]
      [`((require . ,search-items) . ,more)
        (parse-search-items
          (append items-so-far
                  (map make-required (parse-search-items '() search-items)))
          more)]
      [`((prefer . ,search-items) . ,more)
        (parse-search-items
          (append items-so-far
                  (parse-search-items '() search-items))
          more)]
      ; HACK? Tags are limited to one argument; should we support any number?
      [`((tagged-with? (,class ,n)) . ,more)
        (parse-search-items
          (cons (prefer-tagged* class (parse-number n))
                items-so-far)
          more)]
      [else (raise-arguments-error 'make-crawler
        @~a{Invalid search item: @search-items})]))

  (define (parse-number n)
    (match n
      [(? number?) n]
      [`(exact ,n)
        (exact-number* n)]
      [`(inexact ,n)
        (inexact-number* n)]
      [else (raise-arguments-error 'make-crawler
        @~a{Invalid number: @n})]))

  (define-values (ws-nodespecs search-item) (parse-crawler-spec crawler-spec))
  (crawler* crawler-spec ws-nodespecs search-item)) ;TODO initial state

(define (start-crawler g crawler)
  ; find nodes for t and b
  )

(define (crawl g crawler)
  ; search the slipnet one timestep
  )


;; ======================================================================
;;
;; Crawler
;;

;(struct crawler* (target-class item-infos to-reject num-steps-taken found)
(struct crawler* (search-items num-steps-taken ht-activations found)
        #:prefab)

;; items = Listof items to search for, e.g. '((has-operand 3) (has-result 28)).
;(define (make-crawler g target-class items)
;  (let-values ([(to-reject items) (partition reject? items)])
;    (crawler* target-class
;              (map (λ (item) (make-item-info g item))
;                   items)
;              to-reject
;              0
;              (void))))

(define (make-crawler g search-items)
  (crawler* search-items 0 empty-hash (void)))

(define (reject? x)
  (match x
    [`(reject ,_) x]
    [else #f]))

(define (make-item-info g item)
  (item-info* item (make-edge->weight item) (item->activations g item)))

(define (crawl-to-completion g crawler)
  (let/cc return
    (for/fold ([crawler crawler])
              ([t 10])
      (cond
        [(crawler-done? g crawler)
         => (λ (node) (return (struct-copy crawler* crawler
                                           [found node])))]
        [else (crawl-step g crawler)]))))

(define crawler-found crawler*-found)

(define (crawler-done? g crawler)
  (let/cc return
    (for/or ([pair (top-activations crawler num-top-archetypes)])
      (match-define `(,node . ,activation) pair)
      (cond
        [(< activation done-threshold)
         (return #f)]
        [(and
           (node-is-a? g node (crawler*-target-class crawler))
           (>= (items-match g crawler node) 0.9))
         node] ;HACK This is a bug if node is #f
        [else #f]))))

;TODO UT
(define (crawl-step g crawler)
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
         [num-steps-taken (add1 (crawler*-num-steps-taken crawler))]
         [new-crawler (struct-copy crawler* crawler
                                   [item-infos item-infos]
                                   [num-steps-taken num-steps-taken])])
    (observing 't num-steps-taken
      (observe! 'node 'ac (merge-activationss (activationss-of new-crawler))))
    new-crawler))

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

; Returns (Listof (Setof2 nodeid)). These edges only list archetypes, unlike
; edges in graph1.rkt, which also include ports.
(define (make-archetype->edges g)
  (λ (archetype)
    (for/list ([hop (g:port->incident-hops g `(,archetype activation))])
      (match-define `((,k1 ,_) (,k2 ,_)) hop)
      (set k1 k2))))

; crude for now
(define (inexact-metric n m)
  (cond
    [(and (number? n) (number? m))
     (max 0.0 (- 1.0
                 (/ (abs (- n m))
                    10.0)))]
    [else 0.0]))

(define (exact-metric n m)
  (cond
    [(and (number? n) (number? m))
     (if (= n m) 1.0 0.0)]
    [else 0.0]))

(define (rejected? crawler node)
  (define to-reject (crawler*-to-reject crawler))
  (if (null? to-reject)
    #f
    (for/or ([rj to-reject])
      (match-define `(reject ,r) rj)
      (equal? node r))))

(define (items-match g crawler node)
  (if (rejected? crawler node)
    0.0
    (apply * (for/list ([item-info (crawler*-item-infos crawler)])
               (item-match g (item-info*-item item-info) node)))))

(define (item-match g item node)
  (match item
    [`(,class (inexact ,n))
      (define num-for-archetype (archetype->number g class n node))
      (inexact-metric n num-for-archetype)]
    [`(,class ,n)
      (define num-for-archetype (archetype->number g class n node))
      (exact-metric n num-for-archetype)]
    [else 1.0]))

; Returns (g (Setof nodeid) -> flonum?) appropriate for item.
(define (make-edge->weight item)
  (match item
    [`(,class ,n)
      (define metric (match n
                       [(? number?) (curry exact-metric n)]
                       [`(inexact ,n) (curry inexact-metric n)]))
      (λ (g edge)
        (match-define `(,archetype1 ,archetype2) (set->list edge))
        (define num-for-archetype1 (archetype->number g class n archetype1))
        (define num-for-archetype2 (archetype->number g class n archetype2))
        (define sw (max 1.0 (sliplink-weight g archetype1 archetype2 1.0)))
        (define w (cond
                    [(void? num-for-archetype1) sw]
                    [(void? num-for-archetype2) sw]
                    [else (let* ([n1 (metric num-for-archetype1)]
                                 [n2 (metric num-for-archetype2)]
                                 [weight (- (* (+ n1 1.0)
                                               (+ n2 1.0))
                                            1.0)])
                            weight)]))  ; weight will be in range 0.0 .. 3.0
        ;(* w sw)
        w)]
    [else
      (λ (g edge)
        (match-define `(,archetype1 ,archetype2) (set->list edge))
        (sliplink-weight g archetype1 archetype2 0.0)
        1.0)]))

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
                 (inexact-metric target-number tag-n))
               (for/list ([tag (tags-of g node)]
                          #:when (node-is-a? g tag tagclass))
                 (tagspec->number tagclass (tagspec-of g tag)))))

(define (tagspec->number class tagspec)
  (match tagspec
    [`(,tagclass ,n . ,_) #:when (equal? tagclass class) ;TODO inheritance
      n]
    [else (void)]))

;(module+ test
;  (require "numbo1.rkt")
;
;  (test-case "edge->weight"
;    (define edge->weight (make-edge->weight '(result 28)))
;    (define g std-numbo-graph)
;    (define w1 (edge->weight g (set 'archetype-result-28 'archetype-28)))
;    (define w2 (edge->weight g (set 'archetype-result-28 '4*7=28)))
;    (define w3 (edge->weight g (set 'archetype-has-operand-3 '3*8=24)))
;    (define w4 (edge->weight g (set 'archetype-has-operand-3 '3*4=12)))
;    (define w5 (edge->weight g (set 'archetype-24 '3*8=24))) ;inexact but good
;    (define w6 (edge->weight g (set 'archetype-12 '3*4=12)))
;    (check >= w1 3.0)
;    (check >= w2 3.0)
;    (check >= w3 1.0)
;    (check >= w4 1.0)
;    (check < w5 w1)
;    (check < w3 w5)
;    (check < w6 w5)
;    (check-equal? w6 0.0)))

;; ======================================================================
;;
;; Ancillary functions for spreading activation
;;

(define (spread-activation-for-item-info g archetype->edges item-info)
  (spread-activation-with-splitting
                     archetype->edges
                     (curry (item-info*-edge->weight item-info) g)
                     (item-info*-activations item-info)))

; archetype->edges: (nodeid -> (Listof (Setof nodeid)))
; edge->weight: (graph* (Setof2 nodeid) -> flonum?)
(define (spread-activation archetype->edges edge->weight initial-activations)
  (define (spread-activation-across-edge activations edge)
    (match-define `(,k1 ,k2) (set->list edge))
    (define weight (edge->weight edge))
    ;#R (list k1 k2 weight)
    ;#R (list (hash-ref activations k1 (void)))
    ;#R (list (hash-ref activations k2 (void)))
    (let* ([activations (add-activation activations k1 k2 weight)]
           [activations (add-activation activations k2 k1 weight)])
      activations))

  (define (add-activation activations from to edge-weight)
    (define r (* 0.2 (- (random) 0.5) edge-weight))
    (define delta (* spread-rate
                     (+ edge-weight r)
                     (hash-ref initial-activations from 0.0)))
    (hash-update activations
                 to
                 (λ (old) (unsafe-sat+ old delta))
                 0.0))

  (define edges (for*/fold ([st empty-set])
                           ([archetype (hash-keys initial-activations)]
                            [e (archetype->edges archetype)])
                  (set-add st e)))

  (for*/fold ([activations (decay-activations initial-activations
                                              spread-decay)])
             ([edge edges])
    (spread-activation-across-edge activations edge)))

(define (other-elem st elem)
  (define ls (set->list st))
  (if (equal? (first ls) elem)
    (second ls)
    (first ls)))

; archetype->edges: (nodeid -> (Listof (Setof nodeid)))
; edge->weight: (graph* (Setof2 nodeid) -> flonum?)
(define (spread-activation-with-splitting
          archetype->edges edge->weight initial-activations)
  (define (spread-activation-from-node activations node)
    (define edges (for*/fold ([st empty-set])
                             ([e (archetype->edges node)])
                    (set-add st e)))
    (define fan-out-factor (/ 1.0 (set-count edges)))

    (for*/fold ([activations activations])
               ([edge edges])
      (define weight (edge->weight edge))
      (define r (* 0.2 (- (random) 0.5) weight))
      (define delta (* spread-rate
                       (+ weight r)
                       fan-out-factor
                       (hash-ref initial-activations node 0.0)))
      (define neighbor (other-elem edge node))
      ;#R (list node neighbor delta)
      (hash-update activations
                   neighbor
                   (λ (old-activation) (unsafe-sat+ old-activation delta))
                   0.0)))

  (for*/fold ([activations (decay-activations initial-activations
                                              spread-decay)])
             ([node (hash-keys initial-activations)])
    (spread-activation-from-node activations node)))


(define (items->activations g items)
  (for/fold ([ht empty-hash])
            ([item items])
    (cond
      [(is-archetype? g item)
       (hash-update ht item (curry + 1.0) 0.0)]
      [else
       (hash-update ht (f:archetype-name item) (curry + 1.0) 0.0)])))

(define (item->activations g item)
  (let ([item (match item
                [`(,class (inexact ,n)) `(,class ,n)]
                [else item])])
    (cond
      [(is-archetype? g item)
       (hash item 1.0)]
      [else
       (hash (f:archetype-name item) 1.0)])))

(define (merge-activationss activationss)
  (apply hash-union activationss #:combine +))

(define (top-archetypes activations [n num-top-archetypes])
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

;(define os (make-hash)) ; observations: key -> (t -> value)
;
;(define (deposit! k t v)
;  (hash-update! os
;                k
;                (λ (ht-t->v)
;                     (hash-set! ht-t->v t v)
;                     ht-t->v)
;                (λ () (make-hash))))
;
;(define (merged-activations crawler)
;  (merge-activationss (for/list ([item-info (crawler*-item-infos crawler)])
;                        (item-info*-activations item-info))))
;
;(define (deposit-activations! crawler)
;  (define t (crawler*-num-steps-taken crawler))
;  (for ([(k v) (merged-activations crawler)])
;    (deposit! k t v))
;  crawler)
;
;(define (timeseries k)
;  (sorted-by-car
;    (hash-ref os k empty-hash)))
;
;; Plot activation history
;(define (plot-ah . ks)
;  (define hts (for/list ([k ks]) (hash-ref os k empty-hash)))
;  (define tmin (apply min-key hts))
;  (define tmax (apply max-key hts))
;  (define ymin (safe-min 0.0 (apply min-value hts)))
;  (define ymax (safe-max 1.0 (apply max-value hts)))
;  (define (make-plot k)
;    (define vs (reverse (for/list ([t-v (timeseries k)])
;                          (list (car t-v) (cdr t-v)))))
;    (plot-pict (lines vs) #:height 100
;                          #:x-label #f #:y-label (~a k)
;                          #:x-min tmin #:x-max tmax
;                          #:y-min ymin #:y-max ymax))
;  (apply vl-append (map make-plot ks)))

; ----------------------------------------------------------------------

;(require "numbo1.rkt")
;
;(define c (make-crawler g '((has-operand 3) (has-operand 8) (result 28))))
;
;(define a1 (do-slipnet-timestep g (items->activations g '((has-operand 3)))))
;(define a2 (do-slipnet-timestep g (items->activations g '((has-operand 8)))))
;(define a3 (do-slipnet-timestep g (items->activations g '((result 28)))))
;
;(define as (top-archetypes (merge-activationss (list a1 a2 a3))))
;
;(define d (time
;  (for/fold ([c c])
;            ([i 50])
;    ;(displayln @~a{timestep @i})
;    (define c* (crawl-step g c))
;    ;(pretty-print (top-activations c* 10))
;    ;(newline)
;    c*
;    )
;  ))
;
;(define ii (third (crawler*-item-infos c)))
;(define f (item-info*-edge->weight ii))
;(f g (set 'archetype-result-25 '28-3=25))
