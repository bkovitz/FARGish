; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang debug typed/racket

(require syntax/parse syntax/parse/define syntax/stx
         (for-syntax "wheel.rkt" racket/syntax syntax/parse syntax/stx
                     syntax/id-table racket/function
                     (only-in sugar slice-at) mischief/sort))
(require "types.rkt" "typed-wheel.rkt")

; Generate make-node in the main macro?

(struct Nodeclass ([name : Symbol]
                   ;[args : (Listof Symbol)]
                   [parents : (Listof Symbol)])
                  #:transparent)

(struct FARGishSpec ([class->parents : (Hashof Symbol (Setof Symbol))])
                    #:transparent)

(begin-for-syntax
  (define (attr-hash . args)
    (apply hasheq args))

  (define (maybe-missing stx)
    (if stx
      stx
      (syntax-property #'(void) 'missing? #t)))

  (define (missing? stx)
    (syntax-property stx 'missing?))

  ; Compile-time Nodeclass
  (struct CNodeclass (name parents attrs) #:transparent)
  ; name : Symbol
  ; parents : (Listof Symbol)
  ; attrs : (Listof Syntax)

  (define (items->fid-table ->k ->v items)
    (for/fold ([ft (make-immutable-free-id-table)])
              ([item items])
      (free-id-table-set ft (->k item) (->v item))))

  (define (make-CNodeclass name parents . attrs)
    (let ([attr-pairs (for/list ([kv (slice-at attrs 2)]
                                 #:when (not (missing? (cadr kv))))
                        kv)])
      (CNodeclass name
                  parents
                  (items->fid-table car cadr attr-pairs))))

  (define (inherit-CNodeclass parent-cn cn)
    (struct-copy CNodeclass cn
                 [attrs (fidtable-merge (CNodeclass-attrs parent-cn)
                                        (CNodeclass-attrs cn))]))

  (define (fidtable-merge ft0 ft1)
    (for/fold ([ft0 ft0])
              ([k (free-id-table-keys ft1)])
      (free-id-table-set ft0 k (free-id-table-ref ft1 k))))

  ;(: merge-from-ancestors (All (A) A (-> A (Listof A)) (-> A A A) -> A))
  (define (merge-from-ancestors item item->parents parent+item->item)
    (let ([ancestor-seq (topological-sort (list item) item->parents)])
      (cond
        [(null? ancestor-seq) item]
        [else (let loop ([parent (car ancestor-seq)]
                         [ancestor-seq (cdr ancestor-seq)])
                (cond
                  [(null? ancestor-seq) (parent+item->item parent item)]
                  [else (loop (parent+item->item parent (car ancestor-seq))
                              (cdr ancestor-seq))]))])))

  (define (nodeclass-inheritance cnodeclasses)
    (let ([name->cnodeclass (let ([ft (items->fid-table CNodeclass-name
                                                        identity
                                                        cnodeclasses)])
                              (λ (name) (free-id-table-ref ft name)))]
          [cn->parents (λ (cn)
                         (map name->cnodeclass
                              (stx->list (CNodeclass-parents cn))))]
          [cn->updated
            (λ (cn) (merge-from-ancestors cn cn->parents inherit-CNodeclass))])
      (for/list ([cn cnodeclasses])
        (let ([cn (cn->updated cn)])
          (with-syntax ([name (CNodeclass-name cn)]
                        [parents (CNodeclass-parents cn)]
                        [((attr-name attr-expr) ...) (free-id-table-map
                                                       (CNodeclass-attrs cn)
                                                       (λ kv kv))])
            #'(name parents ((attr-name attr-expr) ...)))))))

;  (define (get-parents nc)
;    (with-syntax ([nc nc])
;      #'(nc.parent ...)))
  )

;(define-syntax (farg-model-spec stx)
(define-syntax (define-spec stx)
  (define-syntax-class arg+type
    #:description "argument"
    #:datum-literals [:]
    (pattern [argname:id : type:expr]))

  (define-syntax-class name+args
    #:description "name maybe with arguments"
    #:attributes [name (arg 1) (arg.type 1)]
    (pattern name:id
             #:with (arg ...) #'()
             #:with (arg.type ...) #'())
    (pattern (name:id arg:arg+type ...)))

  (define-syntax-class nodeclass-head
    #:datum-literals [nodeclass tagclass]
    #:attributes [tagclass?]
    (pattern nodeclass
             #:with tagclass? #'#f)
    (pattern tagclass
             #:with tagclass? #'#t))

  (define-syntax-class applies-to
    #:datum-literals [applies-to]
    (pattern (applies-to ~! (x:expr ...)
                         )))

  (define-syntax-class nodeclass
    #:description "nodeclass"
    #:attributes [name (arg 1) (arg.type 1) (parent 1) value display-name
                  tagclass? cnodeclass]
    (pattern (head:nodeclass-head ~! decl:name+args body:nodeclass-body)
             #:with name #'decl.name
             #:with (arg ...) #'(decl.arg ...)
             #:with (arg.type ...) #'(decl.arg.type ...)
             #:with (parent ...) #'(body.parent ... ...)
             ;#:attr sortable (SortNodeclass (syntax-e #'name) (map syntax-e (stx->list #'(parent ...))))
             #:with value #'(~? body.value-expr #f)
             #:with display-name
                      (if (syntax-property #'body.display-name-expr 'missing?)
                        #''name
                        #'body.display-name-expr)
             #:with tagclass? #'head.tagclass?
             #:attr cnodeclass
                      (make-CNodeclass #'name
                                       #'(parent ...)
                                       #'vlue #'body.value-expr
                                       #'dsplay-name #'body.display-name-expr)
             ;#:do [(println (attribute sortable))]
;             #:do [(println (list (syntax->datum #'tagclass?)
;                                  (void? (syntax->datum #'tagclass?))))]
    ))

  (define-splicing-syntax-class nodeclass-body
    #:datum-literals [is-a archetype value links-into display-name]
    #:attributes [(parent 2) value-expr display-name-expr]
    (pattern (~seq
      (~alt (is-a ~! parent:expr ...+)
            (~optional (value ~! value-expr:expr)
                       #:too-many "'value' specified more than once"
                       #:defaults ([value-expr
                                     (syntax-property #'(void) 'missing? #t)]))
            (~optional (display-name ~! display-name-expr:expr)
                       #:too-many "'display-name' specified more than once"
                       #:defaults ([display-name-expr
                                     (syntax-property #'(void) 'missing? #t)]))
            (~optional applies-to-expr:applies-to
                       #:too-many "'applies-to' specified more than once"
                       )
            ) ...)
       #:do [(println #'(~? (parent ... ...) #f))]))

  (syntax-parse stx
    [(_ spec-name:id nc:nodeclass ...)
;     #:with (nc-struct ...)
;            #'((Nodeclass 'nc.name '(nc.arg ...) (list nc.parent ...)) ...)
;     #`(list nc-struct ...)
     ;#:with ht/class->is-a (format-id stx "ht/class->is-a" #:source stx)
;     #:attr sorted-nodeclasses (topological-sort #'(nc.name ...)
;                                                 (λ (name)
;                                                   #'(nc.parents ...) for that name))
;     #:attr ht/name->parents
;      (apply hash (stx->list #'((~@ nc.name (nc.parent ...)) ...)))
;     #:attr topo (stx-map (λ (nc) nc) ;get-parents ;(λ (nc) (with-syntax ([nc #'nc])
;                                    ;#'(nc.parent ...)))
;                          #'((list nc.parent ...) ...))
;     ;#:attr topo #'(nc.parent ...)
     #:attr topo (attribute nc.cnodeclass)
     #:do [(displayln (format "topo: ~a" (attribute topo)))
           (displayln (nodeclass-inheritance (attribute nc.cnodeclass)))]
     #`(begin
         (begin (: nc.name : nc.arg.type ... -> (Hashof Symbol Any))
                (define (nc.name nc.arg ...)
                  (let ([dispname : DisplayName nc.display-name]
                        [tag? : Boolean nc.tagclass?])
                    (hash 'class 'nc.name
                          'value nc.value
                          'display-name dispname
                          'tag? tag?
                          )))) ...
         (: spec-name : FARGishSpec)
         (define spec-name
           (FARGishSpec (hash (~@ 'nc.name (set 'nc.name 'nc.parent ...)) ...)))
         )]))

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

(define-spec spec
  (nodeclass A
    (display-name 'this-is-A))
  (nodeclass B
    (is-a A))
  (nodeclass C
    (is-a A)
    (display-name 'this-is-C))
  (nodeclass D
    (is-a C)))
