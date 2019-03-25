; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang debug typed/racket

(require syntax/parse syntax/parse/define syntax/stx
         (for-syntax "wheel.rkt" racket/syntax syntax/parse syntax/stx
                     syntax/id-table racket/function
                     (only-in "typed-wheel.rkt" merge-from-ancestors)
                     (only-in sugar slice-at) mischief/sort))
(require "typed-wheel.rkt")
(require "types.rkt" "id-set.rkt" "fizzle.rkt")
;(module+ test (require typed/rackunit phc-toolkit/typed-rackunit))

(provide define-spec
         empty-spec
         nodeclass-is-a?
         ->spec
         class->taggee-infos
         class->condition
         (struct-out FARGishSpec)
         (struct-out Graph)
         (struct-out TaggeeInfo)
         ApplyTag)

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

(define-type HasTag? (-> Graph Node Boolean))
; (has-tag? g node)

(define-type TaggeeMultiplicity (U Exact-Positive-Integer 'any))

;(define-type TagCondition (-> Graph (U Node Void) (U Any #f)))
(define-type TagCondition
  (-> Graph (Maybe Node) (Hashof Symbol MaybeNodes) (U Any #f)))
; The third argument of a TagCondition maps the taggee names to the taggees.

(struct Nodeclass ([name : Symbol]
                   ;[args : (Listof Symbol)]
                   [parents : (Listof Symbol)])
                  #:transparent)

(struct TaggeeInfo ([name : Symbol]
                    [from-port-label : Port-label]
                    [to-port-label : Port-label]
                    [of-classes : (Listof Symbol)]
                    [multiplicity : TaggeeMultiplicity])
                   #:transparent)

(struct FARGishSpec ([ht/class->parents : (Hashof Symbol (Setof Symbol))]
                     [ht/class->apply-tag : (Hashof Symbol ApplyTag)]
                     [ht/class->taggee-infos :
                       (Hashof Symbol (Listof TaggeeInfo))]
                     [ht/class->condition : (Hashof Symbol TagCondition)])
                    #:transparent)

(define empty-spec (FARGishSpec (hash) (hash) (hash) (hash)))

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
  (struct CNodeclass (name args parents taggee-infos condition apply-tag attrs)
                     #:transparent)
  ; name : Symbol
  ; args : (Listof ??)
  ; parents : (Listof Symbol)
  ; taggee-infos : (Listof TaggeeInfo)
  ; condition : TagCondition
  ; apply-tag : ApplyTag
  ; attrs : (Listof Syntax)

  (define tag-condition-always-true #'(ann (const #t) TagCondition))

  (define (items->free-id-table ->k ->v items)
    (for/fold ([ft (make-immutable-free-id-table)])
              ([item items])
      (free-id-table-set ft (->k item) (->v item))))

  (define (free-id-table-merge ft0 ft1)
    (for/fold ([ft0 ft0])
              ([k (free-id-table-keys ft1)])
      (free-id-table-set ft0 k (free-id-table-ref ft1 k))))

  (define (make-CNodeclass
            name args parents taggee-infos condition apply-tag . attrs)
    (let ([attr-pairs (for/list ([kv (slice-at attrs 2)]
                                 #:when (not (missing? (cadr kv))))
                        kv)])
      (CNodeclass name
                  args
                  parents
                  taggee-infos
                  condition
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
                        [taggee-infos (CNodeclass-taggee-infos cn)]
                        [condition (CNodeclass-condition cn)]
                        [apply-tag (CNodeclass-apply-tag cn)]
                        [((attr-name attr-expr) ...) (free-id-table-map
                                                       (CNodeclass-attrs cn)
                                                       (λ kv kv))])
            #'(name args parents taggee-infos condition apply-tag
                    ((attr-name attr-expr) ...))))))))

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

  (define-syntax-class multiplicity-value
    #:description "multiplicity"
    (pattern (~or* n:exact-positive-integer
                   (~literal any))))

  ;TODO rm
  (define-splicing-syntax-class taggee-info
    #:datum-literals [by-ports of-class]
    #:attributes [from-port-label to-port-label of-classes multiplicity]
    (pattern (~seq
               (~optional (by-ports ~! from-port-label:id to-port-label:id)
                          ;#:too-many "by-ports' specified more than once"
                          #:defaults ([from-port-label #'tagged]
                                      [to-port-label #'tags]))
               (~optional (of-class ~! class:id ...+))
               (~optional ((~literal multiplicity) mult:multiplicity-value)
                          #:defaults ([mult #'1]))
               )
      #:with of-classes #'(~? (list 'class ...)
                              '())
      #:attr multiplicity (syntax->datum #'mult)
      ))

  ; Example:
  ;  [node1 (by-ports lesser greater) (of-class number) (multiplicity 1)]
  (define-syntax-class taggee-spec
    #:datum-literals [by-ports]
    #:attributes [name from-port-label to-port-label of-classes multiplicity
                  let-binding]
    (pattern
      [name:id (~seq (~alt
                 (~optional (by-ports ~! from-port-label:id to-port-label:id)
                   #:too-many "'by-ports' specified more than once"
                   #:defaults ([from-port-label #'tagged]
                                        [to-port-label #'tags]))
                 (~optional ((~literal of-class) ~! of-class:id ...+)
                   #:too-many "'of-class' specified more than once")
                 (~optional ((~literal multiplicity) ~!
                              multiplicity:multiplicity-value)
                   #:too-many "'multiplicity' specified more than once"
                   #:defaults ([multiplicity #'1]))
                 ) ...)]
      #:with of-classes #'(~? (of-class ...)
                              ())
      #:with let-binding
             #`[name #,(if (equal? 1 (syntax->datum #'multiplicity))
                         #'(ht->node ht/nodes 'name)
                         #'(ht->node/s ht/nodes 'name))]
      ))

  ;TODO I think apply-tag will need to be rewritten so that it applies only
  ;one tag at a time, with the name of the taggee specified. The make-tag
  ;function will have to call apply-tag multiple times. Other code can
  ;add on taggees. tag-info will have to allow specifying multiplicity.
  (define-syntax-class applies-to
    #:datum-literals [applies-to]
    #:attributes [taggee-infos condition mk/apply-tag]
    (pattern (applies-to ~! (taggee:taggee-spec ...)
               (~optional ((~literal condition) c:expr ...+)))
      #:with taggee-infos
             #'(list (TaggeeInfo 'taggee.name
                                 'taggee.from-port-label
                                 'taggee.to-port-label
                                 'taggee.of-classes
                                 'taggee.multiplicity) ...)
      #:with tag-arity #`#,(length (stx->list #'(taggee ...)))
      #:attr mk/apply-tag
             (λ (classname)
               #`(λ ([g : Graph] [tag : Node] [nodes : (Listof Node)])
                    : Graph
                   (cond
                     [(not (= tag-arity (length nodes)))
                      (fizzle:tag-arity #,classname tag-arity 'nodes)]
                     [else
                      (let* ((~@ [node (car nodes)]
                                 [nodes (cdr nodes)]
                                 [g (add-edge g
                                      `((,tag taggee.from-port-label)
                                        (,node taggee.to-port-label)))]) ...)
                              g)])))
      #:with g (format-id this-syntax "g" #:source #'this-syntax)
      #:with this (format-id this-syntax "this" #:source #'this-syntax)
      ;NICE It would be nice if the syntax were defined so that references to
      ;g happened automatically and the condition code didn't have to look
      ;up its own attributes by explicitly going through 'this'.
      #:with condition
             #`(~? (λ ([g : Graph]
                       [this : (U Node Void)]
                       [ht/nodes : (Hashof Symbol MaybeNodes)]) : (U Any #f)
                     (let* (taggee.let-binding ...)
                       c ...))
                   #,tag-condition-always-true)

      ;#:do [(displayln (syntax->datum #'condition))]
      ;#:do [(displayln (syntax->datum #'apply-tag))]
      ;#:do [(displayln (attribute t-info))]
      ))

  (define-splicing-syntax-class nodeclass-body
    #:datum-literals [is-a archetype value links-into display-name]
    #:attributes [(parent 2) value-expr display-name-expr mk/apply-tag
                  taggee-infos condition]
    (pattern (~seq
      (~alt (is-a ~! parent:id ...+)
            (~optional (value ~! value-expr:expr)
                       #:too-many "'value' specified more than once"
                       #:defaults ([value-expr missing]))
            (~optional (display-name ~! display-name-expr:expr)
                       #:too-many "'display-name' specified more than once"
                       #:defaults ([display-name-expr missing]))
            (~optional applies-to-expr:applies-to
                       #:too-many "'applies-to' specified more than once")
            ) ... )
        ;#:do [(displayln (attribute applies-to-expr.mk/apply-tag))]
        #:attr mk/apply-tag
               (or (attribute applies-to-expr.mk/apply-tag)
                   (λ (classname)
                     #`(λ ([g : Graph] [tag : Node] [nodes : (Listof Node)])
                          : Graph
                         (cond
                           [(null? nodes) g]
                           [else (fizzle:not-a-tag #,classname)]))))
        #:with taggee-infos #`(~? applies-to-expr.taggee-infos '())
        #:with condition
               (or (attribute applies-to-expr.condition)
                   tag-condition-always-true)
        ))

  (define-syntax-class nodeclass
    #:attributes [cnodeclass]
    (pattern (head:nodeclass-head ~! decl:name+args body:nodeclass-body)
      #:attr cnodeclass
             (make-CNodeclass #'decl.name
                              #'(decl.arg ...)
                              #'(body.parent ... ...)
                              #'body.taggee-infos
                              #'body.condition
                              ((attribute body.mk/apply-tag) #'decl.name)
                              #'value #'body.value-expr
                              #'display-name #'body.display-name-expr
                              #'tag? #'head.tag?)))

  (syntax-parse stx
    [(_ spec-name:id nc0:nodeclass ...)
     #:with ((nc.name ((nc.arg.name (~literal :) nc.arg.type) ...)
                       (nc.parent ...)
                       nc.taggee-infos
                       nc.condition
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
                   (hash (~@ 'nc.name nc.apply-tag) ...)]
                 [ht/class->taggee-infos : (Hashof Symbol (Listof TaggeeInfo))
                   (hash (~@ 'nc.name nc.taggee-infos) ...)]
                 [ht/class->condition : (Hashof Symbol TagCondition)
                   (hash (~@ 'nc.name nc.condition) ...)])
            (FARGishSpec ht/class->parents
                         ht/class->apply-tag
                         ht/class->taggee-infos
                         ht/class->condition))))]))

(: ->spec : (U Graph FARGishSpec) -> FARGishSpec)
(define (->spec g-or-spec)
  (cond
    [(FARGishSpec? g-or-spec) g-or-spec]
    [else (Graph-spec g-or-spec)]))

(: nodeclass-is-a? :
  (U FARGishSpec Graph) (U Symbol Void) (U Symbol Void) -> Boolean)
(define (nodeclass-is-a? g-or-spec nc ancestor)
  (cond
    [(or (void? nc) (void? ancestor)) #f]
    [(eq? nc ancestor) #t]
    #:define ancestors (hash-ref (FARGishSpec-ht/class->parents
                                   (->spec g-or-spec))
                                 nc
                                 (const (void)))
    [(void? ancestors) #f]
    [else (set-member? ancestors ancestor)]))

(: class->taggee-infos : (U Graph FARGishSpec) Symbol
                         -> (U (Listof TaggeeInfo) Void))
(define (class->taggee-infos g-or-spec class)
  (hash-ref (FARGishSpec-ht/class->taggee-infos (->spec g-or-spec))
            class
            (const (void))))

(: class->condition : (U Graph FARGishSpec) Symbol -> (U Void TagCondition))
(define (class->condition g-or-spec class)
  (hash-ref (FARGishSpec-ht/class->condition (->spec g-or-spec))
            class
            (const (void))))

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

