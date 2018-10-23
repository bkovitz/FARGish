#lang debug at-exp errortrace racket

;; Data structure for port graphs

(require rackunit racket/generic racket/struct "id-set.rkt"
         racket/dict racket/pretty describe mischief/memoize) 
(require racket/serialize)

(provide (struct-out graph)
         empty-graph
         add-spec
         copy-graph-into-graph
         
         make-node
         default-name
         add-node
         add-edge
         remove-node
         
         has-node?
         has-edge?
         all-nodes
         all-edges
         find-nodes-of-class

         set-node-attr
         get-node-attr
         get-node-attrs
         update-node-attr
         node-attr?

         graph-edge-weight

         port->neighboring-ports
         port->neighbors
         port->neighbor
         port->port-label->nodes 
         port->incident-edges

         node->neighbors
         node->ports
         node->incident-edges

         pr-node
         pr-graph
         pr-group

         class-of
         value-of
         value-of-equal?
         
         members-of
         member-of?
         members-of
         member-of
         nodes-of-class-in
         bound-from-ctx-to-ctx?

         next-to?
         bound-to?
         bound-from?
         succ?
         placeholder
         placeholder?
         group?

         graph-set-var
         graph-get-var
         graph-update-var
         graph-push-var
         graph-push-and-set-var
         graph-pop-var)

;; A port graph

(struct graph (ht-node->attrs
               ht-port->neighboring-ports
               edges
               id-set
               stacks  ; dict of temp vars for do-graph-edits
               vars    ; hash-table of vars: name -> value
               spec) #:prefab)   ; #:transparent)

(define empty-spec #hash())
(define empty-graph
  (graph #hash() #hash() #hash() empty-id-set #hash() #hash() empty-spec))

(define (add-spec g spec)
  (struct-copy graph g [spec spec]))

;; Miscellaneous

(define placeholder
  (let ()
     (struct placeholder [])
     (placeholder)))

(define (placeholder? x) (eq? x placeholder))

; --------------------
; Graph variables
; 
; Graph variables are variables associated with the graph as a whole.
; Each variable has its own stack, manipulable by graph-push-var and
; graph-pop-var.
;
; It's not an error to pop an empty stack.

(module+ test
  (test-case "graph variables"
    (let* ([g empty-graph]
           ; Setting, getting, and updating
           [g (graph-set-var g 'abc 5)]
           [_ (check-equal? (graph-get-var g 'abc) 5)]
           [_ (check-equal? (graph-get-var g 'undefined 86) 86)]
           [g (graph-update-var g 'abc add1)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           ; Pushing and popping
           [g (graph-push-var g 'abc)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           [g (graph-set-var g 'abc 'new-value)]
           [_ (check-equal? (graph-get-var g 'abc) 'new-value)]
           [g (graph-pop-var g 'abc)]
           [_ (check-equal? (graph-get-var g 'abc) 6)]
           ; Push and set at the same time
           [g (graph-push-and-set-var g 'xyz 123)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 123)]
           [g (graph-push-and-set-var g 'xyz 456)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 456)]
           [g (graph-pop-var g 'xyz)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) 123)]
           [g (graph-pop-var g 'xyz)]
           [_ (check-equal? (graph-get-var g 'xyz (void)) (void))])
      (void))))

(define (graph-set-var g name value)
  (let ([ht (hash-set (graph-vars g) name value)])
    (struct-copy graph g [vars ht])))

(define graph-get-var
  (case-lambda
    [(g name)
     (hash-ref (graph-vars g) name)]
    [(g name failure-result)
     (hash-ref (graph-vars g) name failure-result)]))

(define graph-update-var
  (case-lambda
    [(g name f)
     (let ([ht (hash-update (graph-vars g) name f)])
       (struct-copy graph g [vars ht]))]
    [(g name f failure-result)
     (let ([ht (hash-update (graph-vars g) name f failure-result)])
       (struct-copy graph g [vars ht]))]))

(define (graph-remove-var g name)
  (let ([vars (hash-remove (graph-vars g) name)])
    (struct-copy graph g [vars vars])))

(define (graph-push-var g name)
  (let ([vars (graph-vars g)])
    (if (hash-has-key? vars name)
      (let* ([value (hash-ref vars name)]
             [stacks (hash-update (graph-stacks g)
                                  name
                                  (λ (stack) (cons value stack))
                                  (λ () '()))])
        (struct-copy graph g [stacks stacks]))
      g)))

(define (graph-push-and-set-var g name value)
  (let ([g (graph-push-var g name)])
    (graph-set-var g name value)))

(define (graph-pop-var g name)
  (let ([stacks (graph-stacks g)])
    (cond
      [(not (hash-has-key? stacks name))
       (graph-remove-var g name)]
      [else
       (let* ([stack (hash-ref stacks name)]
              [value (car stack)]
              [stack (cdr stack)]
              [stacks (if (null? stack)
                        (hash-remove stacks name)
                        (hash-set stacks name stack))]
              [vars (hash-set (graph-vars g) name value)])
         (struct-copy graph g [stacks stacks] [vars vars]))])))

;; Querying a graph

(define (all-nodes g)
  (hash-keys (graph-ht-node->attrs g)))

(define (all-edges g)
  (hash-keys (graph-edges g)))

(define (name-of g node)
  (get-node-attr g node 'name))

(define (class-of g node)
  (get-node-attr g node 'class))

(define (value-of g node)
  (get-node-attr g node 'value))

(define (value-of-equal? g v node)
  (define node-value (value-of g node))
  (and (not (void? node-value)) (equal? v node-value)))

;TODO UT
(define (find-nodes-of-class g class)
  (for/list ([node (all-nodes g)]
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
  (node-attr? g 'group? node))

(define (succ? g node1 node2)
  (for*/or ([succ (port->neighbors g `(,node1 succ-to))]
            [succ-to (port->neighbors g `(,succ succ-to))])
    (equal? succ-to node2)))

;TODO UT
(define (nodes-of-class-in g class groupid)
  (for/list ([node (members-of g groupid)]
             #:when (equal? class (get-node-attr g node 'class)))
    node))

(module+ test
  (define (make-letter-graph . letters)
    (for/fold ([g empty-graph])
              ([letter letters])
      (add-node g `((class . letter) (name . ,letter) (value . ,letter)))))
  (let* ([g (make-letter-graph 'a 'b)]
         [g (add-node g '((class . source) (name . source15)))])
    (check-equal? (find-nodes-of-class g 'source)
                  '(source15))
    (check-equal? (list->set (find-nodes-of-class g 'letter))
                  (set 'a 'b))))

;; Printing a graph

(define (pr-node g nodeid)
  (printf " ~a ~a\n" (~a nodeid #:min-width 12)
                      (hash-remove (get-node-attrs g nodeid) 'id))
  (for* ([port (node->ports g nodeid)]
         [neighboring-port (port->neighboring-ports g port)])
    (define weight (~r (graph-edge-weight g `(,port ,neighboring-port))
                       #:precision '(= 1)))
    (printf "  ~a -- ~a ~a\n" port neighboring-port weight)))

(define (pr-graph g)
  (displayln "Nodes:")
  (for ([nodeid (sort (set->list (all-nodes g))
                      (λ (id1 id2) (string<? (~a id1) (~a id2))))])
    (pr-node g nodeid)))

(define (pr-group g groupid)
  (displayln "Nodes:")
  (for ([nodeid (cons groupid
                      (sort (members-of g groupid)
                            (λ (id1 id2) (string<? (~a id1) (~a id2)))))])
    (pr-node g nodeid)))

(define (has-node? g id)
  (hash-has-key? (graph-ht-node->attrs g) id))

(module+ test
  (check-false (has-node? empty-graph 'plus)))

(define letter-symbols
  (set 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v
       'w 'x 'y 'z))

(define (letter-symbol? x)
  (set-member? letter-symbols x))

;; A HACK for now. This should fill in the attrs with defaults from the
;; class definition in the spec. If attrs is just a symbol or number,
;; we should find an appropriate class definition. For now, though, we
;; just hard-code a couple things.
(define (normalize-attrs attrs)
  (define (recur attrs)
    (match attrs
      [(? list?)
       (recur (make-immutable-hash attrs))]
      [(hash-table ('class cl) _ ...)
       attrs]
      [(? letter-symbol? letter)
       (recur `((class . letter) (value . ,letter)))]
      [(? symbol? sym)
       (recur `((class . ,sym)))]
      [(? number? n)
       (recur `((class . number) (value . ,n) (name . ,n)))]
      [(hash-table _ ...)
       (raise-arguments-error 'normalize-attrs "must define 'class"
                              "attrs" attrs)]
      [_ (raise-arguments-error 'normalize-attrs "invalid node attributes"
                                "attrs" attrs)]))
  (ensure-has-name (recur attrs)))

(define (ensure-has-name attrs)
  (match attrs
    [(hash-table ('name _) _ ...)
     attrs]
    [_ (hash-set attrs 'name (default-name attrs))]))

(define (value-name attrs)
  (match attrs
    [(hash-table ('value (? placeholder? p)) _ ...)
     'placeholder]
    [(hash-table ('value (? pair? v)) _ ...)
     (string->symbol (string-join (map ~a v) "-"))]
    [(hash-table ('value v) _ ...)
     v]
    [(hash-table ('class c) _ ...)
     c]))

(define (default-name attrs) ; N.b.: ignores 'name key even if it's there
  (match attrs
    [(hash-table ('class class) ('tag? #t) _ ...)
     (value-name attrs)]
    [(hash-table ('class 'letter) _ ...)
     (value-name attrs)]
    [(hash-table ('class 'number) _ ...)
     (value-name attrs)]
    [(hash-table ('class class) ('value (list v ...)) _ ...)
     (string->symbol (string-join (cons (~a class) (map ~a v)) "-"))]
    [(hash-table ('class class) ('value v) _ ...)
     (string->symbol (format "~a~a" class v))]
    [(hash-table ('class class) _ ...)
     class]))

(module+ test
  (check-equal? (normalize-attrs 'a)
                #hash((class . letter) (value . a) (name . a)))
  (check-equal? (normalize-attrs '((class . plus)))
                #hash((class . plus) (name . plus)))
  (check-equal? (normalize-attrs '((class . plus) (value . xyz)))
                #hash((class . plus) (value . xyz) (name . plusxyz)))
  (check-equal? (normalize-attrs '((class . fills-port)
                                   (tag? . #t)
                                   (value . (fills-port 5 result))))
                #hash((class . fills-port)
                      (tag? . #t)
                      (name . fills-port-5-result)
                      (value . (fills-port 5 result)))))

;; Making nodes

(define (make-node g attrs) ;returns g* id  (two values)
  (let*-values ([(attrs) (normalize-attrs attrs)]
                [(name) (hash-ref attrs 'name)]
                [(id-set id) (gen-id (graph-id-set g) name)]
                [(attrs) (hash-set attrs 'id id)]
                [(g) (struct-copy graph g
                       [ht-node->attrs
                         (hash-set (graph-ht-node->attrs g) id attrs)]
                       [id-set id-set])])
    (values g id)))

;; Returns void if either node or key not found.
(define (get-node-attr g id k)
  (let ([hm (get-node-attrs g id)])
    (if (void? hm)
      (void)
      (hash-ref (get-node-attrs g id) k (void)))))
;  (match (graph-ht-node->attrs g)
;    [(hash-table ((== id) attrs) _ ...)
;     (hash-ref attrs k (void))]
;    [_ (void)]))

;;TODO UT
;; Returns value of id's attribute k, or #f if either node or key not found
(define (node-attr? g k id)
  (let ([ht (get-node-attrs g id)])
    (if (void? ht) #f (hash-ref ht k #f))))
;  (match (get-node-attrs g id)
;    [(hash-table ((== k) v) _ ...)
;     v]
;    [_ #f]))

;;TODO UT
(define (get-node-attrs g id) ;returns void if node not found
  (hash-ref (graph-ht-node->attrs g) id (void))
  #;(match (graph-ht-node->attrs g)  ;OUCH!!! SLOW
    [(hash-table ((== id) attrs) _ ...)
     attrs]
    [_ (void)]))

;TODO UT
(define (set-node-attr g node k v)
  (if (has-node? g node)
    (let ([attrs (get-node-attrs g node)])
      (struct-copy graph g
        [ht-node->attrs
          (hash-set (graph-ht-node->attrs g) node (hash-set attrs k v))]))
    g))

;TODO UT
(define (update-node-attr g node k f failure-result)
  (if (has-node? g node)
    (let* ([attrs (get-node-attrs g node)]
           [attrs (hash-update attrs k f failure-result)])
      (struct-copy graph g
        [ht-node->attrs
          (hash-set (graph-ht-node->attrs g) node attrs)]))
    g))

(module+ test
  (let*-values ([(g target15) (make-node empty-graph '((class . target15)))]
                [(g target15a) (make-node g '((class . target15)))])
    (check-equal? target15 'target15)
    (check-true (has-node? g 'target15))
    (check-equal? target15a 'target15a)
    (check-true (has-node? g 'target15a))
    (check-equal? (get-node-attr g 'target15 'name) 'target15)
    (check-equal? (get-node-attr g 'target15 'class) 'target15)
    (check-equal? (get-node-attr g 'target15 'id) 'target15)
    (check-equal? (get-node-attr g 'target15a 'name) 'target15)
    (check-equal? (get-node-attr g 'target15a 'class) 'target15)
    (check-equal? (get-node-attr g 'target15a 'id) 'target15a)
    (check-pred void? (get-node-attr g 'no-such-node 'id))
    (check-pred void? (get-node-attr g 'target15 'no-such-attr))))

(define (add-node g attrs) ;returns g*  (doesn't tell caller assigned id)
  (let-values ([(g id) (make-node g attrs)])
    g))

(module+ test
  (let* ([g (add-node empty-graph 'plus)]
         [g (add-node g 'plus)])
    (check-true (has-node? g 'plus))
    (check-true (has-node? g 'plus2))))

;;; Making edges

;edge is '((node1 port-label1) (node2 port-label2))
;Doesn't add the edge if it already exists, but will change its weight.
(define (add-edge g edge [weight 1.0])
  (match-define `(,port1 ,port2) edge)
  (define edges (graph-edges g))
  (let* ([p->nps (graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps port1 (λ (st) (set-add st port2)) (set))]
         [p->nps (hash-update p->nps port2 (λ (st) (set-add st port1)) (set))])
    (struct-copy graph g
      [edges (hash-set edges (set port1 port2) weight)]
      [ht-port->neighboring-ports p->nps]
      )))

;TODO Should accept a list or a set for 'edge'
(define (has-edge? g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (set port1 port2))
  (hash-has-key? (graph-edges g) edge*))

(define (graph-edge-weight g edge)
  (let ([edge (if (set? edge) (set->list edge) edge)])
    (match-define `(,port1 ,port2) edge)
    (define edge* (set port1 port2))
    (hash-ref (graph-edges g) edge* (void))))

(define (remove-edge g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (apply set edge))
  (let* ([p->nps (graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps
                              port1
                              (λ (st) (set-remove st port2))
                              (set))]
         [p->nps (hash-update p->nps
                              port2
                              (λ (st) (set-remove st port1))
                              (set))])
    (struct-copy graph g
      [edges (hash-remove (graph-edges g) edge*)]
      [ht-port->neighboring-ports p->nps])))

;TODO UT
(define (remove-node g node)
  (let ([g (for/fold ([g g])
                     ([edge (node->incident-edges g node)])
             (remove-edge g edge))])
    (struct-copy graph g
      [ht-node->attrs (hash-remove (graph-ht-node->attrs g) node)])))

(module+ test
  (test-case "add-edge"
    (let* ([g (add-node empty-graph '((class . number) (name . source9)))]
           [g (add-node g '((class . plus)))]
           [g (add-edge g '((source9 output) (plus operand)))])
      (check-true (has-edge? g '((source9 output) (plus operand))))
      (check-true (has-edge? g '((plus operand) (source9 output))))
      (check-equal? (graph-edge-weight g '((plus operand) (source9 output)))
                    1.0)
      (let* ([g (remove-edge g '((plus operand) (source9 output)))])
        (check-false (has-edge? g '((source9 output) (plus operand)))))))
  (let* ([ab-graph (add-node empty-graph '((class . letter) (name . a)))]
         [ab-graph (add-node ab-graph '((class . letter) (name . b)))])
    (test-case "add-edge with weight"
      (let ([g (add-edge ab-graph '((a out) (b in)) 0.62)])
        (check-equal? (graph-edge-weight g '((a out) (b in)))
                      0.62)))
    (test-case "add-edge twice"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.51)]
             [g (add-edge g '((a out) (b in)))])
        (check-equal? (list (set '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (graph-edge-weight g `((a out) (b in)))
                      1.00)))
    (test-case "add-edge twice with weight"
      (let* ([g (add-edge ab-graph '((a out) (b in)) 0.75)]
             [g (add-edge g '((a out) (b in)) 0.22)])
        (check-equal? (list (set '(a out) '(b in)))
                      (all-edges g))
        (check-equal? (graph-edge-weight g `((a out) (b in)))
                      0.22)))
  ))

;;; Neighbors

(define (port->neighboring-ports g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port '()))

(define empty-set (set))

;TODO refactor
(define (port->neighbors g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (for/list ([neighboring-port (in-set (hash-ref p->nps port empty-set))])
    (match-define (list neighbor _) neighboring-port)
    neighbor))

(define (port->neighbor g port)
  (match (port->neighbors g port)
    ['() (void)]
    [`(,neighbor . ,_) neighbor]))

(define (port-neighbor? g port node)
  (for/or ([neighbor (port->neighbors g port)])
    (equal? neighbor node)))

;TODO Inefficient
(define (node->ports g node)
  (for/list ([port (hash-keys (graph-ht-port->neighboring-ports g))]
             #:when (equal? node (car port)))
    port))

;Returns sets
(define (port->incident-edges g port)
  (for/list ([nport (port->neighboring-ports g port)])
    (set port nport)))

;Returns a set of nodes
(define (port->port-label->nodes g from-port to-port-label)
  (for/fold ([nodes empty-set])
            ([nport (port->neighboring-ports g from-port)])
    (match-define `(,nport-node ,nport-label) nport)
    (if (equal? to-port-label nport-label)
      (set-add nodes nport-node)
      nodes)))

;Returns lists; first port of edge is the node's port
;TODO OAOO
(define (node->incident-edges g node)
  (for*/list ([port (node->ports g node)]
              [nport (port->neighboring-ports g port)])
    `(,port ,nport)))

(define (node->neighbors g node)
  (for*/set ([port (node->ports g node)]
             [neighbor (port->neighbors g port)])
    neighbor))

(module+ test
  (let* ([g (make-letter-graph 'a 'b 'c)]
         [g (add-edge g '((a out) (b in)))]
         [g (add-edge g '((a out) (c in)))])
    (check-equal? (list->set (port->neighbors g '(a out)))
                  (set 'b 'c))
    (let* ([g (remove-edge g '((b in) (a out)))])
      (check-equal? (port->neighbors g '(a out)) '(c)))))

;TODO Replace dict-ref with hash-ref
(define (map-edge node-map edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (define new-node1 (dict-ref node-map node1))
  (define new-node2 (dict-ref node-map node2))
  `((,new-node1 ,port-label1) (,new-node2 ,port-label2)))

(define (copy-graph-into-graph g g1)
  (let-values ([(g node-map)
      (for/fold ([g g] [node-map #hash()])
                ([node (all-nodes g1)])
        (let-values ([(g nodeid) (make-node g (get-node-attrs g1 node))])
          (values g (hash-set node-map node nodeid))))])
    (let ([g (for/fold ([g g])
                        ([edge (all-edges g1)])
                (let ([g-edge (map-edge node-map (set->list edge))])
                  (if (has-edge? g g-edge)
                    g
                    (add-edge g g-edge (graph-edge-weight g1 edge)))))])
      (values g node-map))))

(module+ test
  (test-case "copy-graph-into-graph"
    (let*-values ([(g) (add-node empty-graph '((class . letter) (name . a)))]
                  [(g) (add-node g '((class . letter) (name . b)))]
                  [(g) (add-edge g '((a out) (b in)))]
                  [(g* node-map) (copy-graph-into-graph empty-graph g)]
                  [(g** node-map2) (copy-graph-into-graph g* g)])
      (check-equal? (list->set (all-nodes g**))
                    (set 'a 'b 'a2 'b2))
      (check-equal? (list->set (all-edges g**))
                    (set (set '(a out) '(b in))
                         (set '(a2 out) '(b2 in)))))))
