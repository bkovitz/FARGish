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

(struct nodeclass* (name parents default-attrs links-intos) #:prefab)

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
    [(list? name) (car name)]
    [else name]))

(define (make-nodeclass name . elems)
  (for/fold ([parents empty-set]
             [default-attrs (hash 'class name)]
             [links-intos empty-set]
             #:result (nodeclass* name
                                  parents
                                  default-attrs
                                  (set->list links-intos)))
            ([elem elems])
    (match elem
      [(is-a* parents-)
       (values (set-union parents parents-) default-attrs links-intos)]
      [(default-attrs* attrs)
       (values parents (hash-union default-attrs attrs) links-intos)]
      [(links-into* ctx by-portss)
       (values parents default-attrs (set-add links-intos elem))])))

;(define default-attrs-for-tag
;  (default-attrs* (hash 'tag? #t)))

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

(struct taggee* (name of-classes by-portss) #:prefab)

(struct of-class* (class) #:prefab)

(define of-class of-class*)

(define (make-taggee name . taggee-infos)
  (define-values (of-classes by-portss)
    (for/fold ([of-classes '()] [by-portss '()])
              ([taggee-info taggee-infos])
      (cond
        [(of-class*? taggee-info)
         (values (cons taggee-info of-classes) by-portss)]
        [(by-ports*? taggee-info)
         (values of-classes (cons taggee-info by-portss))]
        [else (raise-arguments-error 'applies-to
                @~a{@taggee-info is neither of-class nor by-ports.})])))
  (taggee* name of-classes by-portss))

(define-syntax applies-to
  (syntax-rules (condition)
    [(applies-to ([taggee taggee-info ...] ...)
       (condition condition-expr) ...)
     (applies-to* (list (make-taggee 'taggee taggee-info ...) ...)
                  (list (make-condition-func taggee ...)))]))

; Makes a function that returns a g-func that returns true if the condition
; applies to the given nodes. TODO: Check preconditions from tag-infos.
(define-syntax make-condition-func
  (syntax-rules ()
    [(make-condition-func (taggee ...) body0 body ...)
     (let ([num-taggees (length (list taggee ...))]
           [make-pred/g (λ (taggee ...) body0 body ...)])
       (λ nodes
         (if (not (= num-taggees (length nodes)))
           (λ (g) #f)  ; tag can't apply if number of nodes is wrong
           (apply make-pred/g nodes))))]))

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

(define (nodeclass*-of g node)
  (hash-ref (get-nodeclasses g) (g:class-of g node)))
  ;TODO Appropriate response if unknown class

(define (get-links-into g node ctx)
  (define nc (nodeclass*-of g node))
  (define ctx-class (g:class-of g ctx))
  (filter (λ (li) (nodeclass-is-a? g (links-into*-ctx-class li) ctx-class))
          (nodeclass*-links-intos nc)))

(define as-member (by-ports 'members 'member-of))

(define spec
  (farg-model-spec
    (nodeclass ws
      (is-a 'ctx))
    (nodeclass number)
    (nodeclass brick
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (tagclass (need source)
      (is-a 'problem-tag))
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
     (g:make-node g default-attrs)
     #;(g:make-node g (hash 'class classname))]
    [else
     (g:make-node g (hash-set default-attrs 'value value))
     #;(g:make-node g (hash 'class classname 'value value))]))

;; Returns two values: g nodeid
(define/g (make-node/in g ctx . args)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (apply make-node g args)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (links-into*-by-portss links-into)])
      (match-define (by-ports* from-port to-port) by-ports)
        (g:add-edge g `((,ctx ,from-port) (,node ,to-port))))))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr)
    (λ (result . ignored) result)))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (add-node/in g . args)
  (first-value (apply make-node/in g args)))

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
