; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang typed/racket

(require syntax/parse syntax/parse/define syntax/stx
         (for-syntax "wheel.rkt" racket/syntax syntax/parse syntax/stx mischief/sort))
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

  (struct SortNodeclass (name parents) #:transparent)

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
                  tagclass? sortable]
    (pattern (head:nodeclass-head ~! decl:name+args body:nodeclass-body)
             #:with name #'decl.name
             #:with (arg ...) #'(decl.arg ...)
             #:with (arg.type ...) #'(decl.arg.type ...)
             #:with (parent ...) #'(body.parent ... ...)
             #:attr sortable (SortNodeclass (syntax-e #'name) (map syntax-e (stx->list #'(parent ...))))
             #:with value #'(~? body.value-expr #f)
             #:with display-name
                      (if (syntax-property #'body.display-name-expr 'missing?)
                        #''name
                        #'body.display-name-expr)
             #:with tagclass? #'head.tagclass?
             #:do [(println (attribute sortable))]
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
                       #:defaults ([value
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
     #:attr topo (attribute nc.sortable)
     #:attr htopo (for/hash ([s (attribute topo)])
                    (values (SortNodeclass-name s) (SortNodeclass-parents s)))
     #:do [(displayln (format "topo: ~a" (attribute topo)))
           ;(displayln (SortNodeclass-parents (cadr (attribute topo))))
           ;(displayln (map SortNodeclass-name (attribute topo)))
           ;(displayln (attribute htopo))
           (displayln (topological-sort (hash-keys (attribute htopo))
                                        (hash->f (attribute htopo))))]
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
  (nodeclass A)
  (nodeclass B
    (is-a A))
  (nodeclass C
    (is-a A))
  (nodeclass D
    (is-a C)))
