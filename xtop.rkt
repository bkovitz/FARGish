; Experiment: top-level code for scouts and support

#lang debug at-exp racket

(require "spreading-activation.rkt"
         (prefix-in m: "model1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require "wheel.rkt" predicates sugar)
(require expect/rackunit (only-in rackunit test-case))

(define eqn-desideratum
  '(find ([eqn (spreading-activation 'slipnet)])
     (splice-into 'ws eqn)))

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

(define desideratum-keywords (seteq 'find 'find/build 'splice-into 'bind))

(define (desideratum-keyword? x)
  (set-member? desideratum-keywords x))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (require (prefix-in f: "fargish1.rkt")
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass portclass)
           (only-in "equation.rkt"
             make-equation))

  (define spec
    (farg-model-spec
      (nodeclass (number n)
        (name n)
        (value n))
      (nodeclass (equation nm)
        (is-a 'ctx)
        (name nm))
      (nodeclass +)))

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
