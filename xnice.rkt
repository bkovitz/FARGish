; xnice.rkt -- Experimenting with making nice syntax for specifying a FARG model

#lang debug at-exp racket

(require racket/hash)
(require (prefix-in g: "graph.rkt") (prefix-in g: "make-graph.rkt"))
(require (for-syntax racket/syntax) racket/syntax)
(require debug/repl describe)

(define empty-set (set))
(define empty-hash (hash))

#;(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ...) body0 body ...)
     (with-syntax ([name/g (format-id #'name "~a/g" #'name
                                      #:source #'name #:props #'name)])
       #'(begin
           (define (name g args ...) body0 body ...)
           (define (name/g args ...)
             (λ (g) (name g args ...)))))]))

;; A function whose first argument is a graph, and that returns one or more
;; values, the first of which is the updated graph.
(struct gfunc* (f) #:property prop:procedure 0)

(define-syntax-rule (gλ args body0 body ...)
  (gfunc* (λ args body0 body ...)))

(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ... . optargs) body0 body ...)
     (with-syntax* ([name (syntax-property #'name 'gfunc? #t #t)]
                    [name/g (format-id #'name "~a/g" #'name
                                       #:source #'name #:props #'name)])
       #`(begin
           (define name (gλ (g args ... . optargs) body0 body ...))
           (define (name/g args ... . optargs)
             (gλ (g) #,@(if (null? (syntax->datum #'optargs))
                          #'(name g args ...)
                          #'(apply name g args ... optargs))))))]))

(struct is-a* (parents) #:prefab)
; parents: (Setof Symbol)

(define (is-a . args)
  (is-a* (list->set args)))

(struct by-ports* (from-port to-port) #:prefab)

(define by-ports by-ports*)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define (links-into ctx-class . by-portss)
  (links-into* ctx-class by-portss))

(struct default-attrs* (attrs))
;attrs: (Hash Any Any)

(struct nodeclass* (name parents default-attrs links-intos applies-tos)
                   #:prefab)

(define (nodeclass-name x)
  (if (nodeclass*? x)
    (nodeclass*-name x)
    x))

(define (hash-ref/sk ht key sk fk)
  (let ([value (hash-ref ht key (void))])
    (cond
      [(void? value) (if (procedure? fk) (fk) fk)]
      [else (sk value)])))

(define (get-spec g-or-spec)
  (cond
    [(farg-model-spec*? g-or-spec) g-or-spec]
    [(g:graph? g-or-spec) (g:graph-spec g-or-spec)]
    [else (raise-arguments-error 'get-spec
                                 @~a{Can't get spec from @|g-or-spec|.})]))

(define (nodeclass-is-a? g-or-spec ancestor child)
  (define child-name (nodeclass-name child))
  (define ancestor-name (nodeclass-name ancestor))
  (if (equal? ancestor-name child-name)
    #t
    (let ([spec (get-spec g-or-spec)]
          [ht (farg-model-spec*-ancestors spec)])
      (hash-ref/sk ht child-name
        (λ (st) (set-member? st ancestor-name))
        #f))))

(define (node-is-a? g ancestor node)
  (nodeclass-is-a? g ancestor (g:class-of g node)))

(define (plain-name name)
  (cond
    [(pair? name) (car name)]
    [else name]))

(define (make-nodeclass name . elems)
  (for/fold ([parents empty-set]
             [default-attrs (hash 'class name)]
             [links-intos empty-set]
             [applies-tos empty-set]
             #:result (nodeclass* name
                                  parents
                                  default-attrs
                                  (set->list links-intos)
                                  (set->list applies-tos)))
            ([elem elems])
    (match elem
      [(is-a* parents-)
       (values
         (set-union parents parents-) default-attrs links-intos applies-tos)]
      [(default-attrs* attrs)
       (values
         parents (hash-union default-attrs attrs) links-intos applies-tos)]
      [(links-into* ctx by-portss)
       (values parents default-attrs (set-add links-intos elem) applies-tos)]
      [(applies-to* _ _)
       (values parents default-attrs links-intos (set-add applies-tos elem))])))

(define (make-tagclass name . elems)
  (define default-attrs (default-attrs* (hash 'tag? #t
                                              'value name)))
  (apply make-nodeclass name (cons default-attrs elems)))

(define-syntax-rule (nodeclass name elems ...)
  (make-nodeclass (quote name) elems ...))

(define-syntax-rule (tagclass name elems ...)
  (make-tagclass (quote name) elems ...))

;; Single instance
;(define (tag-applies-to? (need source) . nodes)
;  (let ([pred (λ (node)
;                (no-neighbor-at-port?/g 'source node))])
;    (apply pred nodes)))
;
;;; Assuming only one condition for now
;(define (tag-applies-to? g tagclass . nodes)
;  (let ([condition?/g (apply (tagclass-condition tagclass) nodes)])
;    (condition?/g g)))
;
;(define (tag-applies-to?/g tagclass . nodes)
;  (apply (tagclass-condition tagclass) nodes))
;
;(define (tag-applies-to? g tagclass . nodes)
;  (let ([condition?/g (tagclass-condition tagclass)])
;    (apply condition?/g g nodes)))
;
;(define (tag-applies-to?/g tagclass . nodes)
;  (λ (g) (apply tag-applies-to? g tagclass nodes)))

(struct applies-to* (taggees conditions) #:prefab)
; taggees: (List taggee*)
; conditions: (List cfunc)

(struct taggee* (name of-classes by-portss) #:prefab)
; name: Any
; of-classes: (List of-class*)
; by-portss: (List by-ports*)

(struct of-class* (class) #:prefab)
; class: Any

(define of-class of-class*)

(define (make-taggee name . taggee-infos)
  (define-values (of-classes by-portss)
    (for/fold ([of-classes '()] [by-portss '()])
              ([taggee-info taggee-infos])
      (cond
        [(of-class*? taggee-info)
         (values (cons (of-class*-class taggee-info) of-classes) by-portss)]
        [(by-ports*? taggee-info)
         (values of-classes (cons taggee-info by-portss))]
        [else (raise-arguments-error 'applies-to
                @~a{@taggee-info is neither of-class nor by-ports.})])))
  (taggee* name of-classes by-portss))

(define-syntax applies-to
  (syntax-rules (condition)
    [(applies-to ([taggee taggee-info ...] ...)
       (condition condition-expr0 condition-expr ...) ...)
     (applies-to* (list (make-taggee 'taggee taggee-info ...) ...)
                  (list (make-condition-func (taggee ...)
                          condition-expr0 condition-expr ...) ...))]))

; Makes a function that returns a g-func that returns true if the condition
; applies to the given nodes. TODO: Check preconditions from tag-infos.
(define-syntax make-condition-func
  (syntax-rules ()
    [(make-condition-func (taggee ...) body0 body ...)
     (let ([num-taggees (length (list 'taggee ...))]
           [make-pred/g (λ (taggee ...) body0 body ...)])
       (λ nodes
         (if (not (= num-taggees (length nodes)))
           (λ (g) #f)  ; tag can't apply if number of nodes is wrong
           (apply make-pred/g nodes))))]))

(define (condition-func-passes? g cfunc nodes)
  (define cfunc-takes-g (apply cfunc nodes))
  (cfunc-takes-g g))

(struct farg-model-spec* (nodeclasses ancestors) #:prefab)
; nodeclasses: (Immutable-HashTable Any nodeclass*)
; ancestors: (Immutable-HashTable Any (Setof Any))

(define (make-ancestors-table ht-nodeclasses)
  (define (all-ancestors-of name)
    (let recur ([name name] [result (set)] [already-seen (set name)])
      (hash-ref/sk ht-nodeclasses name
        (λ (nc)
          (let* ([parents (set-subtract (nodeclass*-parents nc) already-seen)]
                 [result (set-union result parents)]
                 [already-seen (set-union already-seen parents)])
            (if (set-empty? parents)
              result
              (apply set-union (map
                                 (λ (parent)
                                   (recur parent result already-seen))
                                 (set->list parents))))))
        result)))
  (for/hash ([name (hash-keys ht-nodeclasses)])
    (values name (set-add (all-ancestors-of name) name))))

(define (farg-model-spec . nodeclasses)
  (define ht (for/hash ([nodeclass nodeclasses])
               (values (nodeclass*-name nodeclass) nodeclass)))
  (farg-model-spec* ht (make-ancestors-table ht)))

(define (get-nodeclasses x)
  (cond
    [(farg-model-spec*? x) (farg-model-spec*-nodeclasses x)]
    [(g:graph? x) (get-nodeclasses (g:graph-spec x))]
    [else (raise-arguments-error 'get-nodeclasses
                                 @~a{Can't get nodeclasses from @|x|.})]))

(define (get-nodeclass* g x)
  (cond
    [(nodeclass*? x) x]
    [else (let ([nodeclasses (get-nodeclasses g)])
            (hash-ref nodeclasses x
                      (λ () (raise-arguments-error 'get-nodeclass*
                              @~a{Unknown node class: @|x|.}))))]))
(define (nodeclass*-of g node)
  (hash-ref (get-nodeclasses g) (g:class-of g node)))
  ;TODO Appropriate response if unknown class

(define (get-links-into g node ctx)
  (define nc (nodeclass*-of g node))
  (define ctx-class (g:class-of g ctx))
  (filter (λ (li) (nodeclass-is-a? g (links-into*-ctx-class li) ctx-class))
          (nodeclass*-links-intos nc)))

(define as-member (by-ports 'members 'member-of))

(define/g (no-neighbor-at-port? g port-label node)
  (null? (g:port->neighbors g `(,node ,port-label))))

(define spec
  (farg-model-spec
    (nodeclass ws
      (is-a 'ctx))
    (nodeclass number)
    (nodeclass brick
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (tagclass (need source)
      (is-a 'problem-tag)
      (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
        (condition (no-neighbor-at-port?/g 'source node))))
    ))

(define start-graph (struct-copy g:graph g:empty-graph [spec spec]))

;; Returns two values: g nodeid
(define/g (make-node g classname [value (void)])
  (define nodeclass (hash-ref (get-nodeclasses g) classname
                              (λ ()
                                (raise-arguments-error 'make-node
                                  @~a{Undefined class name: @|classname|.}))))
  (define default-attrs (nodeclass*-default-attrs nodeclass))
  (cond
    [(void? value)
     (g:make-node g default-attrs)]
    [else
     (g:make-node g (hash-set default-attrs 'value value))]))

;; Returns two values: g nodeid
(define/g (make-node/in g ctx . args)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (apply make-node g args)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (links-into*-by-portss links-into)])
      (match-define (by-ports* from-port to-port) by-ports) ;TODO OAOO
      (g:add-edge g `((,ctx ,from-port) (,node ,to-port))))))

(define/g (link-to g by-portss from-node to-node)
  (let* ([by-portss (match by-portss
                      [(struct* taggee* ([by-portss bps])) bps]
                      [(struct* links-into* ([by-portss bps])) bps]
                      [else
                        (raise-arguments-error 'link-to
                          @~a{Can't extract by-portss from @|by-portss|.})])])
    (for/fold ([g g])
              ([by-ports by-portss])
      (match-define (by-ports* from-port to-port) by-ports)
      (g:add-edge g `((,from-node ,from-port) (,to-node ,to-port))))))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr)
    (λ (result . ignored) result)))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (add-node/in g . args)
  (first-value (apply make-node/in g args)))

(define (possible-taggee? g taggee node)
  (for/or ([of-class (taggee*-of-classes taggee)])
    (node-is-a? g of-class node)))

(define (all-taggees-could-apply? g applies-to nodes)
  (define taggees (applies-to*-taggees applies-to))
  (if (not (= (length taggees) (length nodes)))
    #f
    (for/and ([taggee taggees]
              [node nodes])
      (possible-taggee? g taggee node))))

;; Returns #f or the applies-to*.
(define (applies-to? g applies-to nodes)
  (if (and (all-taggees-could-apply? g applies-to nodes)
           (any-matching-condition? g applies-to nodes))
    applies-to
    #f))

(define (any-matching-condition? g applies-to nodes)
  (for/or ([cfunc (applies-to*-conditions applies-to)])
    (condition-func-passes? g cfunc nodes)))

(define (first-matching-applies-to g tagclass nodes)
  (define nodeclass (get-nodeclass* g tagclass))
  (define applies-tos (nodeclass*-applies-tos nodeclass))
  (for/or ([applies-to applies-tos])
    (applies-to? g applies-to nodes)))

;TODO Make the tag a member of the nodes' least common ctx
;TODO Don't make the tag if it's already there
(define/g (make-tag g tagclass . nodes)
  (cond
    [(first-matching-applies-to g tagclass nodes)
     => (λ (applies-to)
          (let*-values ([(g tag) (make-node g tagclass)])
            (for/fold ([g g] #:result (values g tag))
                      ([taggee (applies-to*-taggees applies-to)]
                       [node nodes])
              (link-to g taggee tag node))))]
    [else (raise 'fizzle)]))

(define/g (add-tag g tagclass . args)
  (first-value (apply make-tag g tagclass args)))

(define (set-intersect* set-or-void . sets)
  (cond
    [(void? set-or-void)
     (apply set-intersect* sets)]
    [(null? sets)
     set-or-void]
    [else (apply set-intersect set-or-void sets)]))

(define (linked-from g taggee-info node)
  (let/cc break
    (for/fold ([froms (void)] #:result (if (void? froms) empty-set froms))
              ([by-ports (in-list (taggee*-by-portss taggee-info))])
      (match-define (by-ports* from-port to-port) by-ports)
      (let ([froms (set-intersect* froms
                                   (g:port->port-label->nodes g
                                                              `(,node ,to-port)
                                                              from-port))])
        (if (set-empty? froms)
          (break froms)
          froms)))))

(define (linked-from-common-node? g taggees nodes)
  (let/cc break
    (for/fold ([back-nodes (void)] #:result (not (set-empty? back-nodes)))
              ([taggee taggees] [node nodes])
      (let ([back-nodes (set-intersect* back-nodes
                                        (linked-from g taggee node))])
        (if (set-empty? back-nodes)
          (break #f)
          back-nodes)))))

(define (tagged-with? g tagclass . nodes)
  (let ([tagclass (get-nodeclass* g tagclass)])
    (for/or ([applies-to (nodeclass*-applies-tos tagclass)])
      (linked-from-common-node? g (applies-to*-taggees applies-to) nodes))))

(define g (void))
(set! g (g:add-spec g:empty-graph spec))

;; A little hack to make it easier to work on graphs in the REPL.
;; (gdo makenode 'ws) operates on g, set!s g to the new graph, and
;; returns the nodeid of the created node.
(define-syntax (gdo stx)
  (syntax-case stx ()
    [(gdo gfunc args ...)
     (with-syntax ([g (format-id #'gdo "g"
                                 #:source #'gdo #:props #'f)])
       #'(call-with-values (λ () (gfunc g args ...))
           (λ (new-g . results)
             (set! g new-g)
             (cond
               [(null? results) (void)]
               [(null? (cdr results)) (car results)]
               [else results]))))]))

(gdo make-node 'ws)
(gdo make-node/in 'ws 'brick 7)
(gdo make-tag '(need source) 'brick7)
(tagged-with? g '(need source) 'brick7)
