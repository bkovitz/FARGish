; fargish.rkt -- Definitions for specifying FARG models
;
; See the unit test at the end for how all the pieces here fit together
; and how to use them.

#lang debug at-exp racket

(require racket/hash)
(require (prefix-in g: "graph.rkt") (only-in "graph.rkt" define/g))
(require "wheel.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require rackunit debug/repl describe)

(provide
  farg-model-spec
  
  ; Spec elements
  is-a by-ports links-into applies-to default-attrs nodeclass tagclass
  of-class

  ; Predefined spec helpers
  as-member no-neighbor-at-port? no-neighbor-at-port?/g

  ; Making and operating on a graph that holds a FARG model
  make-empty-graph
  make-node make-node/g
  make-node/in make-node/in/g
  add-node add-node/g
  add-node/in add-node/in/g
  add-nodes/in add-nodes/in/g
  link-to link-to/g

  node-is-a? nodeclass-is-a?

  add-edge    ; Forwarded from graph.rkt
  class-of
  port->neighbor
  port->neighbors

  ; Specifically related to tagging
  applies-to?
  make-tag make-tag/g
  add-tag add-tag/g
  tagged-with?

  ; Accessing a spec
  get-spec
  )

(define add-edge g:add-edge)
(define class-of g:class-of)
(define port->neighbor g:port->neighbor)
(define port->neighbors g:port->neighbors)

;; ======================================================================
;;
;; Structs that hold a FARG model specification and its elements
;;

(struct farg-model-spec* (nodeclasses ancestors) #:prefab)
; nodeclasses: (Immutable-HashTable Any nodeclass*)
; ancestors: (Immutable-HashTable Any (Setof Any))

(struct is-a* (parents) #:prefab)
; parents: (Setof Symbol)

(struct by-ports* (from-port-label to-port-label) #:prefab)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(struct default-attrs* (attrs))
;attrs: (Hash Any Any)

(struct nodeclass* (name parents default-attrs links-intos applies-tos)
                   #:prefab)

(struct applies-to* (taggee-infos conditions) #:prefab)
; taggees: (List taggee-info*)
; conditions: (List cfunc)

;TODO taggee-info*
(struct taggee-info* (name of-classes by-portss) #:prefab)
; name: Any
; of-classes: (List of-class*)
; by-portss: (List by-ports*)

(struct of-class* (class) #:prefab)
; class: Any

;; ======================================================================
;;
;; Functions and macros to create elements of FARG model specifications
;;
;; A definition of a FARG model specification should consist entirely of
;; invocations of these functions and macros.
;;

(define (farg-model-spec . nodeclasses)
  (define ht (for/hash ([nodeclass nodeclasses])
               (values (nodeclass*-name nodeclass) nodeclass)))
  (farg-model-spec* ht (make-ancestors-table ht)))

(define (is-a . args)
  (is-a* (list->set args)))

(define by-ports by-ports*)

(define (links-into ctx-class . by-portss)
  (links-into* ctx-class by-portss))

(define of-class of-class*)

(define default-attrs default-attrs*)

;TODO Make condition(s) optional.
(define-syntax applies-to
  (syntax-rules (condition)
    [(applies-to ([taggee taggee-elem ...] ...)
       (condition condition-expr0 condition-expr ...) ...)
     (applies-to* (list (make-taggee-info 'taggee taggee-elem ...) ...)
                  (list (make-condition-func (taggee ...)
                          condition-expr0 condition-expr ...) ...))]))

(define-syntax-rule (nodeclass name elems ...)
  (make-nodeclass (quote name) elems ...))

(define-syntax-rule (tagclass name elems ...)
  (make-tagclass (quote name) elems ...))

;; ----------------------------------------------------------------------
;;
;; Handy predefined elements and functions for making a FARG model
;; specification.
;;

(define as-member (by-ports 'members 'member-of))

(define/g (no-neighbor-at-port? g port-label node)
  (null? (g:port->neighbors g `(,node ,port-label))))

;; ======================================================================
;;
;; Non-exported functions and macros that support those above that
;; create elements of a FARG model specification.
;;

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

(define (make-taggee-info name . taggee-elems)
  (define-values (of-classes by-portss)
    (for/fold ([of-classes '()] [by-portss '()])
              ([taggee-elem taggee-elems])
      (cond
        [(of-class*? taggee-elem)
         (values (cons (of-class*-class taggee-elem) of-classes) by-portss)]
        [(by-ports*? taggee-elem)
         (values of-classes (cons taggee-elem by-portss))]
        [else (raise-arguments-error 'applies-to
                @~a{@taggee-elem is neither of-class nor by-ports.})])))
  (taggee-info* name of-classes by-portss))

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

;; ======================================================================
;;
;; Functions to make it easy to pass arguments that are either names of
;; specification elements or the specification elements themselves
;;

(define (nodeclass-name x)
  (if (nodeclass*? x)
    (nodeclass*-name x)
    x))

(define (get-spec g-or-spec)
  (cond
    [(farg-model-spec*? g-or-spec) g-or-spec]
    [(g:graph? g-or-spec) (g:graph-spec g-or-spec)]
    [else (raise-arguments-error 'get-spec
                                 @~a{Can't get spec from @|g-or-spec|.})]))

;; ======================================================================
;;
;; Functions to access elements of a FARG model specification
;;
;; Where appropriate, these functions' first argument can be either a graph
;; (that contains a spec) or a spec.
;;

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

(define (nodeclass-is-a? g-or-spec ancestor child)
  (define child-name (nodeclass-name child))
  (define ancestor-name (nodeclass-name ancestor))
  (if (equal? ancestor-name child-name)
    #t
    (let* ([spec (get-spec g-or-spec)]
           [ht (farg-model-spec*-ancestors spec)])
      (hash-ref/sk ht child-name
        (λ (st) (set-member? st ancestor-name))
        #f))))

(define (node-is-a? g ancestor node)
  (nodeclass-is-a? g ancestor (g:class-of g node)))

;; ======================================================================
;;
;; Functions that create and operate on a graph that holds a FARG model
;;

(define (make-empty-graph spec)
  (struct-copy g:graph g:empty-graph [spec spec]))

;; Returns two values: g nodeid
(define/g (make-node g classname [value (void)])
  (define nodeclass (hash-ref (get-nodeclasses g) classname
                              (λ ()
                                (raise-arguments-error 'make-node
                                  @~a{Undefined class name: @|classname|.}))))
  (define default-attrs (nodeclass*-default-attrs #R nodeclass))
  (cond
    [(void? value)
     (g:make-node g #R default-attrs)]
    [else
     (g:make-node g #R (hash-set default-attrs 'value value))]))

;; Returns two values: g nodeid
(define/g (make-node/in g ctx . args)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (apply make-node g args)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (links-into*-by-portss links-into)])
      (match-define (by-ports* from-port-label to-port-label) by-ports) ;TODO OAOO
      (g:add-edge g `((,ctx ,from-port-label) (,node ,to-port-label))))))

(define/g (link-to g by-portss from-node to-node)
  (let* ([by-portss (match by-portss
                      [(struct* taggee-info* ([by-portss bps])) bps]
                      [(struct* links-into* ([by-portss bps])) bps]
                      [else
                        (raise-arguments-error 'link-to
                          @~a{Can't extract by-portss from @|by-portss|.})])])
    (for/fold ([g g])
              ([by-ports by-portss])
      (match-define (by-ports* from-port-label to-port-label) by-ports)
      (g:add-edge g `((,from-node ,from-port-label) (,to-node ,to-port-label))))))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (add-node/in g . args)
  (first-value (apply make-node/in g args)))

(define/g (add-nodes/in g ctx classname vs)
  (for/fold ([g g])
            ([value vs])
    (add-node/in g ctx classname value)))

;; ----------------------------------------------------------------------
;;
;; Graph functions specifically relating to tags
;;

;TODO Make the tag a member of the nodes' least common ctx
;TODO Don't make the tag if it's already there
(define/g (make-tag g tagclass . nodes)
  (cond
    [(first-matching-applies-to g tagclass nodes)
     => (λ (applies-to)
          (let*-values ([(g tag) (make-node g tagclass)])
            (for/fold ([g g] #:result (values g tag))
                      ([taggee-info (applies-to*-taggee-infos applies-to)]
                       [node nodes])
              (link-to g taggee-info tag node))))]
    [else (raise 'fizzle)]))

(define/g (add-tag g tagclass . args)
  (first-value (apply make-tag g tagclass args)))

(define (tagged-with? g tagclass . nodes)
  (let ([tagclass (get-nodeclass* g tagclass)])
    (for/or ([applies-to (nodeclass*-applies-tos tagclass)])
      (linked-from-common-node? g (applies-to*-taggee-infos applies-to) nodes))))

(define (linked-from g taggee-info node)
  (let/cc break
    (for/fold ([froms (void)] #:result (if (void? froms) empty-set froms))
              ([by-ports (in-list (taggee-info*-by-portss taggee-info))])
      (match-define (by-ports* from-port-label to-port-label) by-ports)
      (let ([froms (set-intersect* froms
                                   (g:port->port-label->nodes g
                                     `(,node ,to-port-label)
                                     from-port-label))])
        (if (set-empty? froms)
          (break froms)
          froms)))))

;; ----------------------------------------------------------------------
;;
;; Internal support functions for the above
;;

(define (condition-func-passes? g cfunc nodes)
  (define cfunc-takes-g (apply cfunc nodes))
  (cfunc-takes-g g))

(define (possible-taggee? g taggee-info node)
  (for/or ([of-class (taggee-info*-of-classes taggee-info)])
    (node-is-a? g of-class node)))

(define (all-taggee-infos-could-apply? g applies-to nodes)
  (define taggee-infos (applies-to*-taggee-infos applies-to))
  (if (not (= (length taggee-infos) (length nodes)))
    #f
    (for/and ([taggee-info taggee-infos]
              [node nodes])
      (possible-taggee? g taggee-info node))))

;; Returns #f or the applies-to*.
(define (applies-to? g applies-to nodes)
  (if (and (all-taggee-infos-could-apply? g applies-to nodes)
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

(define (linked-from-common-node? g taggee-infos nodes)
  (let/cc break
    (for/fold ([back-nodes (void)] #:result (not (set-empty? back-nodes)))
              ([taggee-info taggee-infos] [node nodes])
      (let ([back-nodes (set-intersect* back-nodes
                                        (linked-from g taggee-info node))])
        (if (set-empty? back-nodes)
          (break #f)
          back-nodes)))))

;; ======================================================================
;;
;; Unit test that shows how all the elements fit together
;;

(module+ test
  (require (only-in "graph.rkt" gdo))

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

  (define g (make-empty-graph spec))

  (define ws (gdo make-node 'ws))
  (check-true (g:has-node? g ws))

  (define brick7 (gdo make-node/in ws 'brick 7))
  (check-true (g:has-node? g brick7))
  (define number15 (gdo make-node/in ws 'number 15))
  (check-true (g:has-node? g number15))

  (define tag (gdo make-tag '(need source) brick7))
  (check-true (tagged-with? g '(need source) brick7))
  (check-false (tagged-with? g '(need source) number15))
  )
