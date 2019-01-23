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

; Returns (Listof (Cons node salience))
(define dragnet->salience-pairs hash->list)

(define (ht-item->scout->initial-dragnet ht/item)
  (match (hash-ref ht/item 'dragnet)
    [`(spreading-activation ,root)
      (λ (g scout)
        ; TODO Get archetypes from 'start-from
        (hash) ;STUB
        )]
    [`(crawl ,ctx)
      (let ([initial-nodes (hash-ref ht/item 'start-from '())])
        (λ (g scout)
          (for/hash ([node initial-nodes])
            (values node 1.0))))]))  ; TODO salience instead of 1.0

(define (ht-item->dragnet->t+1 ht/item)
  (match (hash-ref ht/item 'dragnet)
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
  (let* ([orig-p1 p1]
         [p1 (quantify-promisingness p1)]
         [p2 (quantify-promisingness p2)])
    (cond
      [(void? p1) #f]
      [(void? p2)
         (not (eq? 'failed orig-p1))]
      [else (>= p1 p2)])))

(define (promisingness<? p1 p2)
  (not (promisingness>=? p1 p2)))

; Makes comparing promisingnesses easier. 'failed = -1.0, 'succeeded = +2.0.
(define (quantify-promisingness p)
  (cond
    [(number? p) (unit-clamp p)]
    [(eq? 'failed p) -1.0]
    [(eq? 'succeeded p) +2.0]
    [else p]))

;; ======================================================================
;;
;; Candidates
;;

(define empty-candidates empty-hash)

(define (candidate->promisingness candidates node)
  (hash-ref candidates node (void)))

(define has-candidate? hash-has-key?)

(define (add-candidate candidates node promisingness)
  (hash-set candidates node (clamp 0.0 0.4 promisingness)))

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
      [else
        (let ([num-ok (add1 num-ok)])
          (if (>= num-ok 2)
            #f
            (loop (cdr pairs) num-ok)))])))

(define (ht-item->candidates->dragnet->candidates ht/item)
  (let* ([salience-threshold (hash-ref ht/item 'salience-threshold)]
         [classes (hash-ref ht/item 'of-class)]
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
             (loop (cdr dragnet-pairs) (add-candidate candidates node))]
            [else (loop (cdr dragnet-pairs) candidates)]))))))

;; ======================================================================
;;
;; Search-items
;;

(struct item-state* (name dragnet candidates dragnet->t+1
                     candidates->dragnet->candidates) #:prefab)

(define (initial-item-state g scout item)
  (let* ([ht/item (parse-search-item item)]
         ;;; Make closures from ht/item
         [scout->initial-dragnet (ht-item->scout->initial-dragnet ht/item)]
         [dragnet->t+1 (ht-item->dragnet->t+1 ht/item)]
         [candidates->dragnet->candidates
           (ht-item->candidates->dragnet->candidates ht/item)]
         ;;; Make data elements of item-state*
         [name (hash-ref ht/item 'name)]
         [dragnet (scout->initial-dragnet g scout)])
    (item-state* name dragnet empty-candidates dragnet->t+1
                 candidates->dragnet->candidates)))

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

; Returns (Hashof symbol Any), with defaults filled in.
;TODO UT
;(define (parse-search-item item)
;  (match item
;    [`(,name ,infos ...)
;      (for/fold ([ht (hash 'name name)] #:result (add-search-item-defaults ht))
;                ([info infos])
;        (let-values ([(k v) (parse-search-item-info info)])
;          (if (hash-has-key? ht k)
;            ht   ; Only first definition of a key is saved
;            (hash-set ht k v))))]
;    [else (raise-arguments-error 'parse-search-item
;            @~a{Invalid search item: @item})]))

;NEXT Make start-dragnet closure from whole desideratum

; Returns (Hashof symbol Any), with defaults filled in.
(define (parse-search-item item)
  (match item
    [`(,name ,infos ...)
      (define ht (infos->ht infos search-item-defaults))
      (hash-set ht 'name name)]
    [else (raise-arguments-error 'parse-search-item
            @~a{Invalid search item: @item})]))

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

; TODO Check validity?
;(define (parse-search-item-info info)
;  (values (car info) (cdr info)))

(define search-item-defaults
  (hash 'dragnet '(spreading-activation 'slipnet)))

;(define (add-search-item-defaults ht)
;  (hash-merge search-item-defaults ht))

;(define (initial-item-state g scout item)
;  (define ht (parse-search-item item))
;  (define name (hash-ref ht 'name))
;  (define dragnet-spec (hash-ref ht 'dragnet))
;  (item-state* name (start-dragnet g scout dragnet-spec) (set)))

;; ======================================================================
;;
;; Scout code
;;

;(define (make-scout g desideratum)
;  (let*-values ([(g scout) (m:make-node/in g 'ws 'scout desideratum)]
;                [(items) (desideratum->items desideratum)]
;                [(item-states) (for/list ([item items])
;                                 (initial-item-state g scout item))]
;                [(g) (m:set-node-attr g scout 'item-states item-states)])
;    (values g scout)))

(define (scout->t+1 g scout)
  ;TODO
  )

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
