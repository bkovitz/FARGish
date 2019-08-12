; Experiments: parsing desiderata

#lang debug at-exp racket

(require "spreading-activation.rkt"
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require "wheel.rkt" predicates)
(require expect/rackunit (only-in rackunit test-case))

(provide desideratum->items)

;; ======================================================================
;;
;; Parsing a desideratum
;;

(define (desideratum->items desideratum)
  (match desideratum
    [`(find (,items ...) ,body ...)
      items]
    [`(find/build (,items ...) ,body ...)
      items]
    [else (raise-arguments-error 'desideratum->items
            @~a{Invalid desideratum: @desideratum})]))

(define (desideratum->followup-definitions desideratum)
  (match desideratum
    [`(,let-like (,items ...) ,body ...)
      body]
    [else (raise-arguments-error 'desideratum->followup-definitions
            @~a{Invalid desideratum: @desideratum})]))

(define desideratum-keywords (seteq 'find 'find/build 'splice-into 'bind))

(define (desideratum-keyword? x)
  (set-member? desideratum-keywords x))

(define (followup-definition->arg-names followup-definition)
  (filter (not? desideratum-keyword?) 
          (flatten (remove-quoted followup-definition))))

(define (splice-into->actions g owner follow-up)
  (let ([state (follow-up*->state follow-up)])
    (case state
      [(starting) `((start-follow-up ,owner follow-up))]
      [(failed) '()]
      [(succeeded) `(scout-
      [else (cond
              [(number? promisingness)
               `((give-support ,owner ,node))]
              [else (raise-arguments-error ...)])])
  (let ([promisingness (follow-up->promisingness
        (cond
          [(eq? 'failed)
           `(cancel-follow-up 
    [(bind)
      (λ (promisingness)
        'STUB


(define ht/follow-up-types
  (hash 'splice-into splice-into->actions
        'bind bind->actions))

(define (follow-up->actions g owner follow-up)
  (let* ([followup-definition (follow-up*-definition followup)]
         [followup-type (followup-definition->type followup-definition)]
         [->actions (hash-ref ht/follow-up-types followup-type (void))])
    (cond
      [(void? ->actions)
       (raise-arguments-error 'follow-up->actions
          @~a{Undefined follow-up type @followup-type in:
              @followup-definition})]
      [else (->actions g owner follow-up)])))

;; ======================================================================
;;
;; Advancing a dragnet
;;

(define (item->dragnet->t+1 item)
  (match (dragnet-type item)
    [`(spreading-activation ,root)
      (λ (g ht/node->activation)
        (spread-activation default-spreading-activation-params
                           ht/node->activation
                           (λ (node)
                             (g:port->neighbors g `(,node activation)))
                           (const 1.0)))]
    [`(crawl ,ctx)
      (λ (g ht/node->salience)
        ;STUB TODO
        ht/node->salience)]
    [else (raise-arguments-error 'item->dragnet->t+1
            @~a{Can't determine dragnet type for item: @item})]))

(define dragnet-types '(spread-activation crawl))

(define (dragnet-type? x)
  (cond
    [(not (pair? x)) #f]
    [else (memq x dragnet-types)]))

(define (dragnet-type item)
  (match item
    [`(,name ,elems ...)
      (findf dragnet-type? (reverse elems))]
    [else (raise-arguments-error 'dragnet-type
            @~a{Can't parse item: @item})]))

;; ======================================================================
;;
;; Advancing a follow-up
;;

(struct follow-up* (definition a/arg->candidate state node) #:prefab)
; definition : The followup-definition that we're implementing
; a/arg->candidate : (Alist name candidate) Arguments, i.e. candidates
;; env : (Hashof name value)  Variables and values (including args)
; state : ?  'starting
; node : (or/c nodeid void)  Node that "does" follow-up, if one has been
;                            created


(define (followup-definition->follow-ups->candidates->follow-ups
          followup-definition)
  (let ([arg-names (followup-definition->arg-names followup-definition)])
    (λ (ht/follow-ups)  ; ht/follow-ups : (Hashof (List followup-definition
                        ;                               arg-alist)
                        ;                         follow-up*)
      (λ (ht/candidates)  ; ht/candidates : (Hashof name (List candidate))
        (let ([arg-alists
                (apply cartesian-product
                       (for/list ([name arg-names])
                         (for/list ([value (hash-ref ht/candidates name)])
                           (cons name value))))])
          (for/fold ([ht/follow-ups ht/follow-ups])
                    ([arg-alist arg-alists])
            (if (follow-up-exists-for?
                  ht/follow-ups followup-definition arg-alist)
              ht/follow-ups
              (add-new-follow-up
                ht/follow-ups followup-definition arg-alist))))))))

(define (add-new-follow-up ht/follow-ups followup-definition arg-alist)
  (hash-set ht/follow-ups
            (list followup-definition arg-alist)
            (follow-up* followup-definition arg-alist 'starting)))

(define (follow-up-exists-for? ht/follow-ups followup-definition arg-alist)
  (hash-has-key? ht/follow-ups (list followup-definition arg-alist)))

; Returns a promisingness or a void (if the node has no 'promisingnessa
; attribute).
(define (follow-up->promisingness g follow-up)
  (let ([state (follow-up*-state follow-up)])
    (case state
      [(starting) 0.1]
      [(failed succeeded) state]
      [(running)
        (let ([node (follow-up*-node follow-up)])
          (g:get-node-attr g node 'promisingness))]
      [else (raise-arguments-error 'follow-up->promisingness
              @~a{invalid follow-up state: @state})])))
              
;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (define d-eqn '(find ([eqn (of-class 'equation)])
                   (splice-into 'ws eqn)))

  (define d-spl
    `(find/build ([9′ (in-ctx 'ws)]
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

  (test-case "followup-definition->arg-names"
    (check-equal? (followup-definition->arg-names '(splice-into 'ws eqn))
                  '(eqn))
    (check-equal? (followup-definition->arg-names '(splice-into ctx1 ctx2))
                  '(ctx1 ctx2)))
  )

;;;;;; REPL code

(define d-eqn '(find ([eqn (of-class 'equation)])
                 (splice-into 'ws eqn)))

(define d-spl
  `(find/build ([9′ (in-ctx 'ws)]
                [4′ (in-ctx 'ws)]
                [5′ (in-ctx 'ws)]
                [+′ (in-ctx 'ws)])
     (bind 9′ 9)
     (bind 4′ 4)
     (bind 5′ 5)
     (bind +′ +)))
