; fargish.rkt -- Definitions for specifying FARG models
;
; See the unit test at the end for how all the pieces here fit together
; and how to use them.

#lang debug at-exp racket

(require racket/hash)
(require (prefix-in g: "graph1.rkt") (only-in "graph1.rkt" define/g gdo))
(require "wheel.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require rackunit debug/repl describe)

(provide
  farg-model-spec
  
  ; Spec elements
  is-a by-ports links-into applies-to default-attrs nodeclass tagclass
  of-class make-nodeclass

  ; Predefined spec helpers
  as-member no-neighbor-at-port? no-neighbor-at-port?/g

  ; Making and operating on a graph that holds a FARG model
  make-empty-graph
  make-node make-node/g
  make-node/in make-node/in/g
  make-node-with-attrs make-node-with-attrs/g
  make-node-with-attrs/in make-node-with-attrs/in/g
  add-node add-node/g
  add-node/in add-node/in/g
  add-nodes/in add-nodes/in/g
  link-to link-to/g

  node-is-a? nodeclass-is-a?

  ; Low-level graph access
  add-edge    ; Forwarded from graph.rkt
  class-of name-of value-of
  tag?
  members-of member-of member-of?
  port->neighbor
  port->neighbors
  find-nodes-of-class
  nodes-of-class-in

  ; Specifically related to tagging
  applies-to?
  make-tag make-tag/g
  add-tag add-tag/g
  tagged-with?

  ; Predefined
  placeholder
  placeholder?

  ; Accessing a spec
  get-spec

  ; Printing
  pr-graph
  pr-node
  pr-group
  )

(define add-edge g:add-edge)
(define get-node-attr g:get-node-attr)
(define port->neighbor g:port->neighbor)
(define port->neighbors g:port->neighbors)

;; ======================================================================
;;
;; Predefined spec elements
;; 

(define placeholder
  (let ()
     (struct placeholder [] #:prefab)
     (placeholder)))

(define (placeholder? x) (eq? x placeholder))

;; ======================================================================
;;
;; Low-level graph-access functions
;; 

(define (name-of g node)
  (get-node-attr g node 'name))

(define (class-of g node)
  (get-node-attr g node 'class))

(define (value-of g node)
  (get-node-attr g node 'value))

(define (value-of-equal? g v node)
  (define node-value (value-of g node))
  (and (not (void? node-value)) (equal? v node-value)))

(define (tag? g node)
  (g:node-attr? g node 'tag?))

;TODO UT
(define (find-nodes-of-class g class)
  (for/list ([node (g:all-nodes g)]
             #:when (equal? class (get-node-attr g node 'class)))
    node))

(define (members-of g groupid)
  (port->neighbors g `(,groupid members)))

(define (member-of g node)
  (port->neighbors g `(,node member-of)))

(define (member-of? g ctx node)
  (member ctx (member-of g node)))

(define (next-to? g . nodes)
  (match nodes
    [`(,a ,b . ,more)
     (for*/or ([seq (port->neighbors g `(,a seq))]
               [seq-next (port->neighbors g `(,seq next))])
       (if (equal? b seq-next)
         (if (null? more) #t (apply next-to? g b more))
         #f))]
    [_ (raise-arguments-error 'next-to? "need at least two nodes"
                              "nodes" nodes)]))

(define (bound-to? g node1 node2)
  (for*/or ([bind (port->neighbors g `(,node1 bound-to))]
            [b-to (port->neighbors g `(,bind bind-to))])
    (equal? b-to node2)))

(define (bound-from? g to-node from-node)
  (for*/or ([bind (port->neighbors g `(,to-node bound-from))]
            [b-from (port->neighbors g `(,bind bind-from))])
    (equal? b-from from-node)))

(define (bound-to g node)
  (for*/list ([bind (port->neighbors g `(,node bound-to))]
              [b-to (port->neighbors g `(,bind bind-to))])
    b-to))

(define (bound-from-ctx-to-ctx? g from-ctx to-ctx from-node)
  (and (member-of? g from-ctx from-node)
       (for/or ([to-node (bound-to g from-node)])
         (member-of? g to-ctx to-node))))

(define (group? g node)
  (g:node-attr? g node 'group?))

(define (succ? g node1 node2)
  (for*/or ([succ (port->neighbors g `(,node1 succ-to))]
            [succ-to (port->neighbors g `(,succ succ-to))])
    (equal? succ-to node2)))

;TODO UT
(define (nodes-of-class-in g class groupid)
  (for/list ([node (members-of g groupid)]
             #:when (equal? class (get-node-attr g node 'class)))
    node))

;; ======================================================================
;;
;; Printing a graph
;; 

(define (pr-node g nodeid)
  (printf " ~a ~a\n" (~a nodeid #:min-width 12)
                      (hash-remove (g:get-node-attrs g nodeid) 'id))
  (for* ([port (g:node->ports g nodeid)]
         [neighboring-port (g:port->neighboring-ports g port)])
    (define weight (~r (g:graph-edge-weight g `(,port ,neighboring-port))
                       #:precision '(= 1)))
    (printf "  ~a -- ~a ~a\n" port neighboring-port weight)))

(define (pr-graph g)
  (displayln "Nodes:")
  (for ([nodeid (sort (set->list (g:all-nodes g))
                      (λ (id1 id2) (string<? (~a id1) (~a id2))))])
    (pr-node g nodeid)))

(define (pr-group g groupid)
  (displayln "Nodes:")
  (for ([nodeid (cons groupid
                      (sort (members-of g groupid)
                            (λ (id1 id2) (string<? (~a id1) (~a id2)))))])
    (pr-node g nodeid)))

;; ======================================================================
;;
;; Structs that hold a FARG model specification and its elements
;;

(struct farg-model-spec* (nodeclasses ancestors) #:prefab)
; nodeclasses: (Immutable-HashTable Symbol nodeclass*)
; ancestors: (Immutable-HashTable Symbol (Setof Symbol))

(struct is-a* (parents) #:prefab)
; parents: (Listof Symbol)

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

(struct archetype* (archetype-type) #:prefab)
; archetype-type: archetype-type?

(define is-value
  (let ()
    (struct is-value* [] #:prefab)
    (is-value*)))

(define (is-value? x) (eq? x is-value))

(define is-node
  (let ()
    (struct is-node* [] #:prefab)
    (is-node*)))

(define (is-node? x) (eq? x is-node))

(define no-archetype
  (let ()
    (struct no-archetype* [] #:prefab)
    (no-archetype*)))

(define (no-archetype? x) (eq? x no-archetype))

(define is-class
  (let ()
    (struct is-class* [] #:prefab)
    (is-class*)))

(define (is-class? x) (eq? x is-class))

(define (archetype-type? x)
  (or (is-value? x) (is-node? x) (no-archetype? x) (is-class? x)))

;; ======================================================================
;;
;; Functions and macros to create elements of FARG model specifications
;;
;; A definition of a FARG model specification should consist entirely of
;; invocations of these functions and macros.
;;



(define (farg-model-spec . nodeclasses)
  (define predefined-nodeclasses  ;TODO move outside
    (list (nodeclass archetype
            (links-into 'slipnet (by-ports 'archetypes 'slipnet) as-member))
          (nodeclass slipnet
            (is-a 'ctx))))
  (let* ([nodeclasses (append predefined-nodeclasses nodeclasses)]
         [ht-nodeclasses (for/hash ([nodeclass nodeclasses])
                           (values (nodeclass*-name nodeclass) nodeclass))]
         [ht-ancestors (make-ancestors-table ht-nodeclasses)]
         [ht-nodeclasses (for/hash ([(name nodeclass) (in-hash ht-nodeclasses)])
                           (values name (inherit-default-attrs
                                          name ht-nodeclasses)))])
    (farg-model-spec* ht-nodeclasses ht-ancestors)))

;  (define ht (for/hash ([nodeclass nodeclasses])
;               (values (nodeclass*-name nodeclass) nodeclass)))
;  (farg-model-spec* ht (make-ancestors-table ht)))

(define (is-a . args)
  (is-a* args))

(define by-ports by-ports*)

(define (links-into ctx-class . by-portss)
  (links-into* ctx-class by-portss))

(define of-class of-class*)

(define archetype archetype*)

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
  (for/fold ([parents '()]
             [default-attrs (hash 'class name)]
             [links-intos empty-set]
             [applies-tos empty-set]
             #:result (nodeclass* name
                                  (remove-duplicates (reverse parents))
                                  default-attrs
                                  (set->list links-intos)
                                  (set->list applies-tos)))
            ([elem elems])
    (match elem
      [(is-a* parents-)
       (values
         (append parents parents-) default-attrs links-intos applies-tos)]
      [(default-attrs* attrs)
       (values
         parents (hash-union default-attrs attrs) links-intos applies-tos)]
      [(links-into* ctx by-portss)
       (values parents default-attrs (set-add links-intos elem) applies-tos)]
      [(applies-to* _ _)
       (values parents default-attrs links-intos (set-add applies-tos elem))]
      [(archetype* atype-type)
       (if (archetype-type? atype-type)
         (values parents (hash-set default-attrs 'archetype-type atype-type)
                 links-intos (set-add applies-tos elem))
         (raise-arguments-error 'nodeclass
           @~a{archetype must be is-value, is-node, or no-archetype,
               not @|atype-type|.}))])))

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
          (let* ([parents (set-subtract (list->set (nodeclass*-parents nc))
                                        already-seen)]
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

; Returns list of ancestors with name first, followed by name's parents
; in the order specified in is-a clauses, followed by the parents of
; each of those, and so on. (Breadth-first traversal.)
(define (ancestors-in-reverse-inheritance-order name ht-nodeclasses)
  (define (recur result pending-parents)
    (cond
      [(null? pending-parents) result]
      [(member (car pending-parents) result)
       (recur result (cdr pending-parents))]
      [else (let* ([parent (car pending-parents)]
                   [nodeclass (hash-ref ht-nodeclasses parent (void))]
                   [grandparents (if (void? nodeclass)
                                   '()
                                   (nodeclass*-parents nodeclass))])
              (recur (cons parent result)
                     (remove-duplicates (append (cdr pending-parents)
                                                grandparents))))]))
  (recur '() (list name)))

(define (inherit-default-attrs nodeclass-name ht-nodeclasses)
  (let* ([nodeclass (hash-ref ht-nodeclasses nodeclass-name)]
         [ancestor-names (ancestors-in-reverse-inheritance-order
                                    nodeclass-name ht-nodeclasses)]
         [ancestor-default-attrs (for/list ([ancestor-name ancestor-names])
                                   (define ancestor (hash-ref ht-nodeclasses
                                                              ancestor-name
                                                              #f))
                                   (if ancestor
                                     (nodeclass*-default-attrs ancestor)
                                     empty-hash))]
         [new-default-attrs
           (apply hash-union ancestor-default-attrs
                             #:combine (λ (v0 v) v))])
    (struct-copy nodeclass* nodeclass
                 [default-attrs new-default-attrs])))

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
  (hash-ref (get-nodeclasses g) (class-of g node)))
  ;TODO Appropriate response if unknown class

(define (get-links-into g node ctx)
  (define nc (nodeclass*-of g node))
  (define ctx-class (class-of g ctx))
  (let ([lis (filter (λ (li)
                       (nodeclass-is-a? g (links-into*-ctx-class li) ctx-class))
               (nodeclass*-links-intos nc))])
    (if (null? lis) (list (links-into ctx as-member)) lis)))

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
  (nodeclass-is-a? g ancestor (class-of g node)))

;; ======================================================================
;;
;; Functions that create and operate on a graph that holds a FARG model
;;

(define (make-empty-graph spec)
  (struct-copy g:graph g:empty-graph [spec spec]))

(define (basic-attrs classname value)
  (if (void? value)
    (hash 'class classname)
    (hash 'class classname 'value value)))

;NEXT Allow name, other overriding attrs, after value.
;; Returns two values: g nodeid
(define/g (make-node g classname [value (void)])
  (make-node-with-attrs g (basic-attrs classname value)))
;  (define nodeclass (hash-ref (get-nodeclasses g) classname
;                              (λ ()
;                                (raise-arguments-error 'make-node
;                                  @~a{Undefined class name: @|classname|.}))))
;  (define default-attrs (nodeclass*-default-attrs nodeclass))
;  (cond
;    [(void? value)
;     (g:make-node g #R default-attrs)]
;    [else
;     (g:make-node g #R (hash-set default-attrs 'value value))]))

;; Returns two values: g nodeid
(define/g (make-node-with-attrs g attrs)
  (define classname (hash-ref attrs 'class
                              (λ ()
                                (raise-arguments-error 'make-node-with-attrs
                                  @~a{No class name provided in @|attrs|.}))))
  (define nodeclass (hash-ref (get-nodeclasses g) classname
                              (λ ()
                                (raise-arguments-error 'make-node-with-attrs
                                  @~a{Undefined class name: @|classname|.}))))
  (define default-attrs (nodeclass*-default-attrs nodeclass))
  (g:make-node g (hash-union default-attrs attrs #:combine (λ (v0 v) v))))

;; Returns two values: g nodeid
(define/g (make-node-with-attrs/in g ctx attrs)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (make-node-with-attrs g attrs)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (links-into*-by-portss links-into)])
      (match-define (by-ports* from-port-label to-port-label) by-ports) ;TODO OAOO
      (g:add-edge g `((,ctx ,from-port-label) (,node ,to-port-label))))))

;; Returns two values: g nodeid
(define/g (make-node/in g ctx classname [value (void)])
  (make-node-with-attrs/in g ctx (basic-attrs classname value)))

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
  (test-case "spec basics"
    (define spec
      (farg-model-spec
        (nodeclass ws
          (is-a 'ctx))
        (nodeclass number
          (archetype is-value))
        (nodeclass brick
          (is-a 'number)
          (links-into 'ctx (by-ports 'bricks 'source) as-member))
        (tagclass (need source)
          (is-a 'problem-tag)
          (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
            (condition (no-neighbor-at-port?/g 'source node))))
        (nodeclass equation
          (is-a 'ctx)
          (archetype is-node))
        ))

    (define g (make-empty-graph spec))

    (define ws (gdo make-node 'ws))
    (check-true (g:has-node? g ws))

    (define brick7 (gdo make-node/in ws 'brick 7))
    (check-true (g:has-node? g brick7))
    (define number15 (gdo make-node/in ws 'number 15))
    (check-true (g:has-node? g number15))

    ;(pr-graph g)

    (define tag (gdo make-tag '(need source) number15))
    (check-true (tagged-with? g '(need source) number15))
    (check-false (tagged-with? g '(need source) brick7))
  )

  (test-case "attribute inheritance"
    (define spec
      (farg-model-spec
        (nodeclass A
          (default-attrs #hash((a . 1) (x . 2))))
        (nodeclass B
          (default-attrs #hash((b . 1) (x . 3))))
        (nodeclass C
          (is-a 'A 'B))  ; last parent takes precedence
        (nodeclass D
          (is-a 'C)
          (default-attrs #hash((a . 4))))))

    (define g (make-empty-graph spec))

    (define a (gdo make-node 'A))
    (check-equal? (get-node-attr g a 'a) 1)
    (check-equal? (get-node-attr g a 'b) (void))
    (check-equal? (get-node-attr g a 'x) 2)

    (define b (gdo make-node 'B))
    (check-equal? (get-node-attr g b 'a) (void))
    (check-equal? (get-node-attr g b 'b) 1)
    (check-equal? (get-node-attr g b 'x) 3)
    
    (define c (gdo make-node 'C))
    (check-equal? (get-node-attr g c 'a) 1)
    (check-equal? (get-node-attr g c 'b) 1)
    (check-equal? (get-node-attr g c 'x) 3)

    (define d (gdo make-node 'D))
    (check-equal? (get-node-attr g d 'a) 4)
    (check-equal? (get-node-attr g d 'b) 1)
    (check-equal? (get-node-attr g d 'x) 3)
    )
  )

;; ======================================================================
;;
;; Archetypes
;; 

(define (archetypes g)
  (port->neighbors g '(slipnet archetypes)))

(define (node->archetype-type g node)
  (get-node-attr g node 'archetype-type no-archetype))

(define (archetype-of-value g value) ;TODO Extremely inefficient
  (let loop ([atypes (archetypes g)])
    (cond
      [(null? atypes)
       #f]
      [(value-of-equal? g value (car atypes))
       (car atypes)]
      [else (loop (cdr atypes))])))

; Makes archetype for node if one does not already exist. Links it to
; 'slipnet node if it's not already linked. Returns two values: g archetype.
; If the node does not get an archetype, returns g <void>.
(define (get-or-make-archetype-for-node g node)
  (define (get-or-make avalue)
    (cond
      [(archetype-of-value g avalue)
       => (λ (found-archetype) (values g found-archetype))]
      [else (make-archetype-for-value g avalue)]))
  (match (node->archetype-type g node)
    [(? no-archetype?) (values g (void))]
    [(? is-node?)
     (values (link-in-new-archetype g node) node)]
    [(? is-value?)
     (get-or-make (value-of g node))]
    [(? is-class?)
     (get-or-make (class-of g node))]))

;TODO Add another nodeclass parameter to specify how to construct a name.
;Then apply it to archetype.
;HACK for now: name it here.

(define (make-archetype-for-value g value)
  (let*-values ([(g archetype) (make-node-with-attrs g
                                 (hash 'class 'archetype
                                       'value value
                                       'name (archetype-name value)))]
                [(g) (link-in-new-archetype g archetype)])
    (values g archetype)))

(define (archetype-name value)
  (string->symbol (string-append "archetype"
    (cond
      [(number? value) (~a value)]
      [(symbol? value) (~a value)]
      [(pair? value) (#\- (string-join (map ~a value) "-"))]
      [else (~a #\- value)]))))

(define (link-in-new-archetype g archetype)
  (let-values ([(g slipnet) (find-or-make-slipnet g)])
    (add-edge g `((,slipnet archetypes) (,archetype slipnet)))))

(define (find-or-make-slipnet g)
  (cond
    [(g:has-node? g 'slipnet) (values g 'slipnet)]
    [else (make-node g 'slipnet)]))

;; ------------------------------------------------------------------------
;;
;; Unit test
;; 

(module+ test
  (test-case "archetypes"
    (define archetype-test-spec
      (farg-model-spec
        (nodeclass ws
          (is-a 'ctx)) ; no archetype
        (nodeclass number
          (archetype is-value))
        (nodeclass operator
          (archetype is-class))
        (nodeclass +
          (is-a 'operator))
        (nodeclass equation
          (is-a 'ctx)
          (archetype is-node))))

    (define g (make-empty-graph archetype-test-spec))

    (define ws (gdo make-node 'ws))
    (define number7 (gdo make-node 'number 7))
    (define plus (gdo make-node '+))
    (define equation (gdo make-node 'equation))

    (define a7 (gdo get-or-make-archetype-for-node number7))
    (define a+ (gdo get-or-make-archetype-for-node plus))
    (define aeq (gdo get-or-make-archetype-for-node equation))
    (define aws (gdo get-or-make-archetype-for-node ws))

    (define a7b (gdo get-or-make-archetype-for-node number7))
    (define a+b (gdo get-or-make-archetype-for-node plus))
    (define aeqb (gdo get-or-make-archetype-for-node equation))

    (check-equal? a7 a7b)
    (check-equal? a+ a+b)
    (check-equal? aeq aeqb)
    (check-equal? aws (void))

    (check-equal? (list->set (archetypes g)) (set a7 a+ aeq))))

