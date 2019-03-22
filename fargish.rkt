; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang debug typed/racket

(require syntax/parse syntax/parse/define syntax/stx
         (for-syntax "wheel.rkt" racket/syntax syntax/parse syntax/stx
                     syntax/id-table racket/function
                     (only-in "typed-wheel.rkt" merge-from-ancestors)
                     (only-in sugar slice-at) mischief/sort))
(require "types.rkt" "typed-wheel.rkt")
(require "id-set.rkt")
;(module+ test (require typed/rackunit phc-toolkit/typed-rackunit))

(provide define-spec empty-spec nodeclass-is-a? (struct-out FARGishSpec)
         (struct-out Graph) ApplyTag)

(struct Graph ([ht-node->attrs : (Hashof Node Attrs)]
               [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
               [ht-edge->weight : (Hashof Edge/UPair EdgeWeight)]
               [id-set : IdSet]
               [stacks : (Hashof Symbol (Listof Any))]
               [vars : (Hashof Symbol Any)]
               [spec : FARGishSpec])
              #:prefab)

; A function that attaches an already-created tag to existing nodes.
(define-type ApplyTag (-> Graph Node (Listof Node) Graph))
; (apply-tag g tag (list taggee1 taggee2 ...))

(struct Nodeclass ([name : Symbol]
                   ;[args : (Listof Symbol)]
                   [parents : (Listof Symbol)])
                  #:transparent)

(struct FARGishSpec ([ht/class->parents : (Hashof Symbol (Setof Symbol))]
                     [ht/class->apply-tag : (Hashof Symbol ApplyTag)])
                    #:transparent)

(define empty-spec (FARGishSpec (hash) (hash)))

(begin-for-syntax
  (define (maybe-missing stx)
    (if stx
      stx
      (syntax-property #'(void) 'missing? #t)))

  (define missing
    (syntax-property #'(void) 'missing? #t))

  (define (missing? stx)
    (syntax-property stx 'missing?))

  ; Compile-time Nodeclass
  (struct CNodeclass (name args parents apply-tag attrs) #:transparent)
  ; name : Symbol
  ; args : (Listof ??)
  ; parents : (Listof Symbol)
  ; attrs : (Listof Syntax)

  (define (items->free-id-table ->k ->v items)
    (for/fold ([ft (make-immutable-free-id-table)])
              ([item items])
      (free-id-table-set ft (->k item) (->v item))))

  (define (free-id-table-merge ft0 ft1)
    (for/fold ([ft0 ft0])
              ([k (free-id-table-keys ft1)])
      (free-id-table-set ft0 k (free-id-table-ref ft1 k))))

  (define (make-CNodeclass name args parents apply-tag . attrs)
    (let ([attr-pairs (for/list ([kv (slice-at attrs 2)]
                                 #:when (not (missing? (cadr kv))))
                        kv)])
      (CNodeclass name
                  args
                  parents
                  apply-tag
                  (items->free-id-table car cadr attr-pairs))))

  (define (inherit-CNodeclass parent-cn cn)
    (struct-copy CNodeclass cn
                 [attrs (free-id-table-merge (CNodeclass-attrs parent-cn)
                                             (CNodeclass-attrs cn))]))

  (define (nodeclass-inheritance cnodeclasses)
    (let ([name->cnodeclass
            (let ([ft (items->free-id-table
                        CNodeclass-name identity cnodeclasses)])
              (λ (name) (free-id-table-ref ft name)))]
          [cn->parents
            (λ (cn) (map name->cnodeclass (stx->list (CNodeclass-parents cn))))]
          [cn->updated
            (λ (cn) (merge-from-ancestors cn cn->parents inherit-CNodeclass))])
      (for/list ([cn cnodeclasses])
        (let ([cn (cn->updated cn)])
          (with-syntax ([name (CNodeclass-name cn)]
                        [args (CNodeclass-args cn)]
                        [parents (CNodeclass-parents cn)]
                        [apply-tag (CNodeclass-apply-tag cn)]
                        [((attr-name attr-expr) ...) (free-id-table-map
                                                       (CNodeclass-attrs cn)
                                                       (λ kv kv))])
            #'(name args parents apply-tag ((attr-name attr-expr) ...))))))))

;(define-syntax (farg-model-spec stx)
(define-syntax (define-spec stx)
  (define-syntax-class arg+type
    #:description "argument with type, like [n : Integer]"
    #:datum-literals [:]
    (pattern [name:id : type:expr]))

  (define-syntax-class name+args
    #:description "name maybe with arguments"
    #:attributes [name (arg 1)]
    (pattern name:id
             #:with (arg ...) #'())
    (pattern (name:id arg:arg+type ...)))

  (define-syntax-class nodeclass-head
    #:datum-literals [nodeclass tagclass]
    #:attributes [tag?]
    (pattern nodeclass
             #:with tag? (syntax-property #'#f 'missing? #t))
    (pattern tagclass
             #:with tag? #'#t))

  (define-splicing-syntax-class taggee-info
    #:datum-literals [by-ports]
    #:attributes [from-port-label to-port-label]
    (pattern (~seq
               (~optional (by-ports ~! from-port-label:id to-port-label:id)
                          ;#:too-many "by-ports' specified more than once"
                          #:defaults ([from-port-label #'tagged]
                                      [to-port-label #'tags])))))

  ;NEXT I think apply-tag will need to be rewritten so that it applies only
  ;one tag at a time, with the name of the taggee specified. The make-tag
  ;function will have to call apply-tag multiple times. Other code can
  ;add on taggees. tag-info will have to allow specifying multiplicity.
  (define-syntax-class applies-to
    #:datum-literals [applies-to]
    #:attributes [apply-tag]
    (pattern (applies-to ~! ([taggee t-info:taggee-info] ...)
                         )
             #:with tag-arity #`#,(length (stx->list #'(taggee ...)))
             #:with apply-tag
             #'(λ ([g : Graph] [tag : Node] [nodes : (Listof Node)])
                  : Graph
                 (cond
                   [(not (= tag-arity (length nodes)))
                    (assert #f)] ;TODO fizzle
                   [else
                    (let* ((~@ [node (car nodes)]
                               [nodes (cdr nodes)]
                               [g (add-edge g
                                    `((,tag t-info.from-port-label)
                                      (,node t-info.to-port-label)))]) ...)
                            g)]))
             #:do [(displayln (syntax->datum #'apply-tag))]
             ))

  (define-splicing-syntax-class nodeclass-body
    #:datum-literals [is-a archetype value links-into display-name]
    #:attributes [(parent 2) value-expr display-name-expr apply-tag]
    (pattern (~seq
      (~alt (is-a ~! parent:expr ...+)
            (~optional (value ~! value-expr:expr)
                       #:too-many "'value' specified more than once"
                       #:defaults ([value-expr missing]))
            (~optional (display-name ~! display-name-expr:expr)
                       #:too-many "'display-name' specified more than once"
                       #:defaults ([display-name-expr missing]))
            (~optional applies-to-expr:applies-to
                       #:too-many "'applies-to' specified more than once")
            ) ... )
      #:with apply-tag 
             #'(~? applies-to-expr.apply-tag
                   (λ ([g : Graph] [tag : Node] [nodes : (Listof Node)])
                      : Graph
                     (cond
                       [(null? nodes) g]
                       [else (assert #f)]))))) ;TODO fizzle

  (define-syntax-class nodeclass
    #:attributes [cnodeclass]
    (pattern (head:nodeclass-head ~! decl:name+args body:nodeclass-body)
      #:attr cnodeclass
             (make-CNodeclass #'decl.name
                              #'(decl.arg ...)
                              #'(body.parent ... ...)
                              #'body.apply-tag
                              #'value #'body.value-expr
                              #'display-name #'body.display-name-expr
                              #'tag? #'head.tag?)))

  (syntax-parse stx
    [(_ spec-name:id nc0:nodeclass ...)
     #:with ((nc.name ((nc.arg.name (~literal :) nc.arg.type) ...)
                       (nc.parent ...)
                       nc.apply-tag
                       ((nc.attr.name nc.attr.expr) ...)) ...)
            (nodeclass-inheritance (attribute nc0.cnodeclass))
     #`(begin
         (local-require "model.rkt")
         (begin
           (: nc.name : nc.arg.type ... -> Attrs)
           (define (nc.name nc.arg.name ...)
             (hash 'class 'nc.name
                   (~@ 'args (list nc.arg.name ...))
                   (~@ 'nc.arg.name nc.arg.name) ...
                   (~@ 'nc.attr.name nc.attr.expr) ...))) ...
         (: spec-name : FARGishSpec)
         (define spec-name
           (let ([ht/class->parents
                   (hash (~@ 'nc.name (set 'nc.name 'nc.parent ...)) ...)]
                 [ht/class->apply-tag
                   (hash (~@ 'nc.name nc.apply-tag) ...)])
            (FARGishSpec ht/class->parents ht/class->apply-tag))))]))

(: nodeclass-is-a? : FARGishSpec (U Symbol Void) (U Symbol Void) -> Boolean)
(define (nodeclass-is-a? spec nc ancestor)
  (cond
    [(or (void? nc) (void? ancestor)) #f]
    [(eq? nc ancestor) #t]
    #:define ancestors (hash-ref (FARGishSpec-ht/class->parents spec)
                                 nc
                                 (const (void)))
    [(void? ancestors) #f]
    [else (set-member? ancestors ancestor)]))

;(spec (nodeclass (blah a b c)))
;
;(spec (nodeclass blah (is-a 'x 'y) (is-a 'z)))
;
;(spec
;  (nodeclass blah
;    (value 22)))

;(define-spec spec
;  (nodeclass (number [n : Integer])
;    (value n)
;    (display-name n)
;    (nodeclass (block [n : Integer])
;      (display-name (format "block ~a" n))))
;  (nodeclass plus)
;  (tagclass greater-than
;    (applies-to ())))
;(number 17)

