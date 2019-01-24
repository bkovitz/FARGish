; Experiment: top-level code for scouts and support

#lang debug at-exp racket

(require (prefix-in sa: "spreading-activation.rkt")
         (prefix-in m: "model1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (only-in "equation.rkt"
           make-equation))
(require "wheel.rkt" racket/hash predicates sugar)
(require expect/rackunit (only-in rackunit test-case))

;; ======================================================================
;;
;; Desideratum functions
;;
;; TODO: move these to a separate module when they work
;;

;TODO Rename to desideratum->search-items
(define (desideratum->items desideratum)
  (match desideratum
    [`(find (,items ...) ,body ...)
      items]
    [else (raise-arguments-error 'desideratum->items
            @~a{Invalid desideratum: @desideratum})]))

(define (desideratum->followup-definitions desideratum)
  (match desideratum
    [`(,let-like (,items ...) ,body ...)
      body]
    [else (raise-arguments-error 'desideratum->followup-definitions
            @~a{Invalid desideratum: @desideratum})]))

(define (splice-into->desideratum spec/splice-into env g)
  (match spec/splice-into
    [`(splice-into ,target-ctx ,source-ctx)
     (let* ([target-ctx (d-subst target-ctx env)]
            [source-ctx (d-subst source-ctx env)]
            [source-nodes (proper-members g source-ctx)]
            [target-node-names (map add-prime source-nodes)]
            [make-target-item (λ (name)
                                `(,name (crawl ,target-ctx)))])
       `(find ,(map make-target-item target-node-names)
          ,@(for/list ([bind-from source-nodes]
                       [bind-to target-node-names])
              `(bind ',bind-from ,bind-to))))]
    [else (raise-arguments-error 'splice-into->desideratum
            @~a{Invalid splice-into: @spec/splice-into})]))

; List of the members of ctx that should be bound from when splicing ctx into
; another ctx. So, probably not tags.
(define (proper-members g ctx)
  (m:members-of g ctx))  ;TODO Be more selective

(define (add-prime x)
  (~> x
      ->string
      (string-append "′")
      string->symbol))

; Replaces variables in d-expr with their definitions in env. If a variable
; is undefined, it's replaced with void. Replaces quotes with the things
; quoted.
(define (d-subst d-expr env)
  (hash-ref env d-expr
    (λ ()
      (cond
        [(void? d-expr) d-expr]
        [(null? d-expr) d-expr]
        [(quote? d-expr) d-expr]
        [(desideratum-keyword? d-expr) d-expr]
        [(pair? d-expr) (map (curryr d-subst env) d-expr)]
        [else (raise-arguments-error 'd-subst
                @~a{Undefined desideratum variable: @d-expr})]))))

(define (quote? x)
  (and (pair? x)
       (eq? 'quote (car x))))

(define desideratum-keywords
  (seteq 'find 'dragnet 'spreading-activation 'crawl 'splice-into 'bind))

(define (desideratum-keyword? x)
  (set-member? desideratum-keywords x))


;; ======================================================================
;;
;; Dragnets
;;
;; A dragnet is a crude search, attracted by salience or activation,
;; capable of finding nodes that have nothing to do with what you're
;; looking for. It's the first stage in a parallel terraced scan,
;; analogous to placing an ad for a job position, which is read by many
;; people who shouldn't even apply.
;;
;; There are two types of dragnet: spreading activation and crawling.

; Returns (Listof (Cons node salience))
(define dragnet->salience-pairs hash->list)

(define (ht-item->scout->initial-dragnet ht/item)
  (match (ht-item-ref ht/item 'dragnet)
    [`(spreading-activation ,root)
      (λ (g scout)
        ; TODO Get archetypes from 'start-from
        (hash) ;STUB
        )]
    [`(crawl ,ctx)
      (let ([initial-nodes (ht-item-ref ht/item 'start-from)])
        (λ (g scout)
          (for/hash ([node initial-nodes])
            (values node 1.0))))]))  ; TODO salience instead of 1.0

(define (ht-item->dragnet->t+1 ht/item)
  (match (ht-item-ref ht/item 'dragnet)
    [`(spreading-activation ,root)
      (λ (g dragnet)
        (define (node->neighbors node)
          (m:port->neighbors g `(,node activation)))
        (sa:spread-activation sa:default-spreading-activation-params
                              dragnet 
                              node->neighbors
                              (const 1.0)))] ; TODO edge weight
    [`(crawl ,ctx)
      (λ (g dragnet)
        dragnet)])) ;STUB Should explore neighbors within ctx

;; ======================================================================
;;
;; Promisingness
;;
;; A promisingness is a number in 0.0..1.0, 'failed, 'succeeded, or void.

(define (promisingness>=? p1 p2)
  (cond
    [(void? p1) #f]
    [(void? p2)
       (not (eq? 'failed p1))]
    [else (>= (quantify-promisingness p1)
              (quantify-promisingness p2))]))

(define (promisingness<? p1 p2)
  (not (promisingness>=? p1 p2)))

; Makes comparing promisingnesses easier. 'failed = -1.0, 'succeeded = +2.0.
(define (quantify-promisingness p)
  (cond
    [(number? p) (unit-clamp p)]
    [(eq? 'failed p) -1.0]
    [(eq? 'succeeded p) +2.0]
    [else p]))

(define (clamp-promisingness p)
  (cond
    [(number? p) (unit-clamp p)]
    [else p]))

(define (clamp-initial-promisingness p)
  (cond
    [(number? p) (clamp 0.0 0.4 p)]
    [else p]))

(define (decay-promisingness p)
  (cond
    [(number? p) (* 0.9 p)]
    [else p]))

;; ======================================================================
;;
;; Candidates
;;
;; A set of candidates is the reasonable nodes culled from a dragnet.
;; The promisingness of candidates is judged based on the promisingness
;; of the follow-ups that depend on them, and decays without promising
;; follow-ups.

(define empty-candidates empty-hash)

(define (candidate->promisingness candidates node)
  (hash-ref candidates node (void)))

(define has-candidate? hash-has-key?)

(define (add-candidate candidates node promisingness)
  (hash-set candidates node (clamp-initial-promisingness promisingness)))

(define (set-candidate-promisingness candidates node promisingness)
  (hash-set candidates node (clamp-promisingness promisingness)))

; Don't advance the dragnet if either one candidate looks very promising
; or at least two candidates look moderately promising.
;TODO Address the hand-chosen constants here in a principled way.
(define (candidates->advance-dragnet? candidates)
  (let loop ([pairs (candidates->promisingness-pairs candidates)]
             [num-ok 0]) ; number of candidates that are moderately promising
    (cond
      [(null? pairs) #t]
      #:define pair (car pairs)
      #:define node (car pair)
      #:define promisingness (cdr pair)
      [(promisingness>=? promisingness 0.8) #f]
      [(promisingness<? promisingness 0.4)
       (loop (cdr pairs) num-ok)]
      [else (let ([num-ok (add1 num-ok)])
              (if (>= num-ok 2)
                #f
                (loop (cdr pairs) num-ok)))])))

; Extracts new candidates from a dragnet and updates the current set of
; candidates.
(define (ht-item->candidates->dragnet->candidates ht/item)
  (let* ([salience-threshold (ht-item-ref ht/item 'salience-threshold)]
         [classes (ht-item-ref ht/item 'of-class)]
         [acceptable? (cond
                        [(empty? classes) ; if no classes, then
                         (const #t)]      ;   every node is acceptable
                        [else
                         (λ (g node)
                           (for/or ([class classes])
                             (m:node-is-a? g node class)))])])
    (λ (old-candidates)
      (λ (g dragnet)
        (let loop ([dragnet-pairs (dragnet->salience-pairs dragnet)]
                   [candidates old-candidates])
          (cond
            [(null? dragnet-pairs)
             candidates]
            #:define pair (car dragnet-pairs)
            #:define node (car pair)
            #:define salience (cdr pair)
            [(< salience salience-threshold)
             candidates]
            [(has-candidate? candidates node)
             (loop (cdr dragnet-pairs) candidates)]
            [(acceptable? g node)
             (loop (cdr dragnet-pairs)
                   (add-candidate candidates node salience))]
            [else (loop (cdr dragnet-pairs) candidates)]))))))

; Returns promisingness of candidate on current timestep
(define (ht-item->follow-ups->candidate->promisingness ht-item)
  (let* ([
  (λ (follow-ups)
    (λ (candidate+promisingness) ; a pair
      (match-define `(,candidate . ,promisingness) candidate+promisingness)
  

;; ======================================================================
;;
;; Search-items
;;
;; A search-item (usually just "item") is something that a scout is
;; looking for. It has a name, some criteria for choosing among nodes,
;; and a way of searching the graph for it. The scout might build a
;; node to fulfill the search-item if necessary.
;;
;; Each search-item has an item-state, which is updated each timestep.
;; The item-state has a dragnet and a current list of candidates.

(struct item-state* (name dragnet candidates follow-ups->item-state->t+1)
                    #:prefab)
;                     follow-ups->candidate->promisingness
;                     dragnet->t+1
;                     candidates->dragnet->candidates) #:prefab)

;(define (initial-item-state g scout item)
;  (let* ([ht/item (parse-search-item item)]
;         ;;; Make closures from ht/item
;         [scout->initial-dragnet (ht-item->scout->initial-dragnet ht/item)]
;         [dragnet->t+1 (ht-item->dragnet->t+1 ht/item)]
;         [candidates->dragnet->candidates
;           (ht-item->candidates->dragnet->candidates ht/item)]
;         ;;; Make data elements of item-state*
;         [name (hash-ref ht/item 'name)]
;         [dragnet (scout->initial-dragnet g scout)])
;    (item-state* name dragnet empty-candidates dragnet->t+1
;                 candidates->dragnet->candidates)))

(define (item->scout->initial-state item)
  (let* ([ht/item (parse-search-item item)]
         [name (hash-ref ht/item 'name)]
         ;;; Make closures from ht/item
         [scout->initial-dragnet (ht-item->scout->initial-dragnet ht/item)]
         [dragnet->t+1 (ht-item->dragnet->t+1 ht/item)]
         [candidates->dragnet->candidates
           (ht-item->candidates->dragnet->candidates ht/item)])
    (λ (g scout)
      (let ([dragnet (scout->initial-dragnet g scout)])
        (item-state* name dragnet empty-candidates dragnet->t+1
                     candidates->dragnet->candidates)))))

(define (item-state->t+1 g item-state)
  (define dragnet (item-state*-dragnet item-state))
  (define old-candidates (item-state*-candidates item-state))
  (define dragnet->t+1 (item-state*-dragnet->t+1 item-state))
  (define candidates->dragnet->candidates
    (item-state*-candidates->dragnet->candidates item-state))
  (if (candidates->advance-dragnet? old-candidates)
    (let* ([dragnet->candidates (candidates->dragnet->candidates
                                  old-candidates)]
           [dragnet (dragnet->t+1 g dragnet)]
           [candidates (dragnet->candidates g dragnet)])
      (struct-copy item-state* [dragnet dragnet]
                               [candidates candidates]))
    item-state))

;NEXT Include the follow-up definitions in ht/item.
(define (ht-item->follow-ups->item-state->t+1 ht/item)
  (let* ([
         [dragnet->t+1 (ht-item->dragnet->t+1 ht/item)]
         [candidates->dragnet->candidates
           (ht-item->candidates->dragnet->candidates ht/item)])
    (λ (follow-ups)
      (λ (item-state)
        (struct-copy item-state* [dragnet dragnet]
                                 [candidates candidates])))))


;TODO Rename to search-item->ht
; Returns (Hashof symbol Any), with defaults filled in.
(define (parse-search-item item)
  (match item
    [`(,name ,infos ...)
      (define ht (infos->ht infos search-item-defaults))
      (hash-set ht 'name name)]
    [else (raise-arguments-error 'parse-search-item
            @~a{Invalid search item: @item})]))

; Looks up a key in a hash table returned by parse-search-item. Undefined
; keys default to a value of '().
(define ht-item-ref (curryr hash-ref '()))

(define (info->kv info)
  (cond
    [(pair? info)
     (values (car info) (cdr info))]
    [else
     (values info '())]))

(define (infos->ht infos [defaults empty-hash])
  (for/fold ([ht empty-hash] #:result (hash-merge defaults ht))
            ([info infos])
    (let-values ([(k v) (info->kv info)])
      (if (hash-has-key? ht k)
        ht   ; Only first definition of a key is saved
        (hash-set ht k v)))))

(define search-item-defaults
  (hash 'dragnet '(spreading-activation 'slipnet)))

;; ======================================================================
;;
;; Scouts
;;
;; A scout is a node that searches for and possibly builds other nodes,
;; seeking to fulfill a desideratum.

;(define (make-scout g desideratum)
;  (let*-values ([(g scout) (m:make-node/in g 'ws 'scout desideratum)]
;                [(items) (desideratum->items desideratum)]
;                [(item-states) (for/list ([item items])
;                                 (initial-item-state g scout item))]
;                [(g) (m:set-node-attr g scout 'item-states item-states)])
;    (values g scout)))

(define (make-scout g desideratum)
  (let* ([items (desideratum->items desideratum)]
         [ls/scout->initial-state (map item->scout->initial-state items)])
    (let*-values ([(g scout) (m:make-node/in g 'ws 'scout desideratum)]
                  [(item-states)
                     (for/list ([scout->initial-state ls/scout->initial-state])
                       (scout->initial-state g scout))]
                  [(g) (m:set-node-attr g scout 'item-states item-states)])
      (values g scout))))

(define (scout->t+1 g scout)
  (let* ([item-states
           (for/list ([item-state (m:get-node-attr g scout 'item-states)])
             (item-state->t+1 g item-state))]
         ;NEXT Start new follow-ups
         [
    ;TODO
    (m:set-node-attr g scout 'item-states item-states)))

; THE RIGHT WAY
; Each timestep, a scout needs to:
;   Update its item-states, i.e. dragnets, candidates, promisingness
;   Start new follow-ups if new candidates necessitate them
;   Build new nodes for any search-items that need them
;   Give support to other nodes
; The closure below does all this by returning a list of actions, not by
; returning an updated graph.
(define (desideratum->scout->actions desideratum)
  (let* (;;; parse
         [items (desideratum->search-items desideratum)]
         [followup-definitions (desideratum->followup-definitions desideratum)]
         [ls/ht/item (map search-item->ht items)]
         ;;; make closures
         [ls/follow-ups->item-state->t+1
           (for/list ([ht/item ls/ht/item])
             (ht-item->follow-ups->item-state->t+1 ht/item))]
         [ls/follow-ups->item-states->starts
           (for/list ([followup-definition followup-definitions])
             (followup-definition->follow-ups->item-states->starts
               followup-definition))])
    (λ (g scout)
      (let* ([old-item-states (m:get-node-attr g scout 'item-states)]
             [old-follow-upss (m:get-node-attr g scout 'follow-upss)]
             [ls/item-states->starts
               (for/list ([f->i->starts ls/follow-ups->item-states->starts]
                          [old-follow-ups old-follow-upss])
                 (f->i->starts old-follow-ups))]
             [new-item-states
               (for/list ([old-item-state old-item-states]
                          [f->i->t+1 ls/follow-ups->item-state->t+1])
                 (let ([item-state->t+1 (f->i->t+1 old-follow-ups)])
                   (item-state->t+1 g old-item-state)))]
             [starts  ; actions to start new follow-ups
               (apply append
                 (for/list ([item-states->starts ls/item-states->starts])
                   (item-states->starts new-item-states)))])
        (list* `(set-attr ,scout item-states ,new-item-states)
               starts)
        ;TODO builds
        ;TODO giving support

;; ======================================================================
;;
;; The top-level code
;;

(define spec
  (farg-model-spec
    (nodeclass (number n)
      (name n)
      (value n))
    (nodeclass (equation nm)
      (is-a 'ctx)
      (name nm))
    (nodeclass +)
    (nodeclass (scout desideratum))
    ))

(define eqn-desideratum
  '(find ([eqn (dragnet (spreading-activation 'slipnet))
               (of-class 'equation)
               (start-from 4 5 15)])
     (splice-into 'ws eqn)))

(define spl-desideratum
  '(find ([9′ (dragnet (crawl 'ws)) (build-from 9)]
          [4′ (dragnet (crawl 'ws))]
          [5′ (dragnet (crawl 'ws))]
          [+′ (dragnet (crawl 'ws)) (build-from +)])
     (bind 9 9′)
     (bind 4 4′)
     (bind 5 5′)
     (bind + +′)))

;(define (run)
;
;  (define g (m:make-empty-graph spec))
;
;  ;(gdo make-scout eqn-desideratum)
;
;  Each timestep, query each node for actions, the coalesce the actions and
;  run them.
;
;  g)

;; REPL code (delete)

(require "shorthand.rkt"
         "equations.rkt")

(define (make-start-g)
  (let*-values ([(g) (make-graph spec '(ws) '(slipnet))]
               [(g) (add-memorized-equations g '((+ 2 3)))])
    g))

(define g (make-start-g))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (define d-eqn '(find ([eqn (of-class 'equation)])
                   (splice-into 'ws eqn)))

  (define d-spl
    `(find ([9′ (in-ctx 'ws)]
            [4′ (in-ctx 'ws)]
            [5′ (in-ctx 'ws)]
            [+′ (in-ctx 'ws)])
       (bind 9′ 9)
       (bind 4′ 4)
       (bind 5′ 5)
       (bind +′ +)))

  (test-case "desideratum->items"
    (check-equal? (desideratum->items d-eqn)
                  '([eqn (of-class 'equation)]))
    (check-equal? (desideratum->items d-spl)
                  '([9′ (in-ctx 'ws)]
                    [4′ (in-ctx 'ws)]
                    [5′ (in-ctx 'ws)]
                    [+′ (in-ctx 'ws)])))
    
  (test-case "desideratum->followup-definitions"
    (check-equal? (desideratum->followup-definitions d-eqn)
                  '((splice-into 'ws eqn)))
    (check-equal? (desideratum->followup-definitions d-spl)
                  '((bind 9′ 9)
                    (bind 4′ 4)
                    (bind 5′ 5)
                    (bind +′ +))))

  (test-case "splice-into->desideratum"
    (define g (m:make-empty-graph spec))
    (gdo make-equation '5 '(+ 2 3))
    (define env (hash 'eqn '2+3=5))
    (define spl '(splice-into 'ws eqn))
    (define result (splice-into->desideratum spl env g))
    (check-equal? result
                  '(find ([3′ (crawl 'ws)]  ; the order is insignificant
                          [5′ (crawl 'ws)]
                          [2′ (crawl 'ws)]
                          [+′ (crawl 'ws)])
                     (bind '3 3′)
                     (bind '5 5′)
                     (bind '2 2′)
                     (bind '+ +′)))
    ))
