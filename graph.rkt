#lang debug at-exp racket

;; Data structure for port graphs

(require rackunit racket/generic racket/struct "id-set.rkt"
         racket/dict racket/pretty describe mischief/memoize) 

(provide make-node add-node add-edge get-node-attr get-node-attrs set-node-attr
         port->neighbors port->neighboring-ports all-nodes
         find-nodes-of-class value-of port->neighbor bound-from-ctx-to-ctx?
         pr-graph pr-group members-of member-of? copy-graph-into-graph
         nodes-of-class-in class-of members-of member-of next-to? bound-to?
         bound-from? succ? has-node? node->neighbors node->ports
         node->incident-edges port->incident-edges has-edge? all-edges
         empty-graph placeholder placeholder? group? remove-node
         graph-set-stacked-variable graph-get-stacked-variable
         graph-push-stacked-variable graph-pop-stacked-variable
         graph-update-stacked-variable node-attr?
         (struct-out graph))

;; A port graph

(struct graph (ht-node->attrs
               ht-port->neighboring-ports
               edges
               id-set
               stacks  ; dict of temp vars for do-graph-edits
               spec) #:transparent)

(define empty-spec '())
(define empty-graph (graph #hash() #hash() (set) empty-id-set '() empty-spec))

;; Stacked variables

(define graph-get-stacked-variable
  (case-lambda
    [(g name default-value)
     (let ([stack (dict-ref (graph-stacks g) name '())])
       (if (null? stack)
         default-value
         (car stack)))]
    [(g name)
     (let ([stack (dict-ref (graph-stacks g) name)])
       (car stack))]))

(define (graph-set-stacked-variable g name value)
  (let* ([stacks (graph-stacks g)]
         [stack (dict-ref stacks name '())]
         [stack (if (null? stack)
                  (list value)
                  (cons value (cdr stack)))])
    (struct-copy graph g
      [stacks (dict-set stacks name stack)])))

(define (graph-update-stacked-variable g name f default-value)
  (define v0 (graph-get-stacked-variable g name default-value))
  (graph-set-stacked-variable g name (f v0)))

(define graph-push-stacked-variable
  (case-lambda
    [(g name value)
     (let* ([stacks (graph-stacks g)]
            [stack (dict-ref stacks name '())]
            [stack (cons value stack)])
       (struct-copy graph g
         [stacks (dict-set stacks name stack)]))]
    [(g name)
     (let* ([stacks (graph-stacks g)]
            [stack (dict-ref stacks name '())]
            [stack (if (null? stack)
                     stack
                     (cons (car stack) stack))])
       (struct-copy graph g
         [stacks (dict-set stacks name stack)]))]))

(define (graph-pop-stacked-variable g name)
  (let* ([stacks (graph-stacks g)]
         [stack (dict-ref stacks name)]
         [stack (if (null? stack) stack (cdr stack))])
    (struct-copy graph g
      [stacks (dict-set stacks name stack)])))

(module+ test
  (test-case "stacked variable"
    (define g0 empty-graph)
    (define g1 (graph-set-stacked-variable g0 'abc 5))
    (check-equal? (graph-get-stacked-variable g1 'abc (void)) 5)
    (define g2 (graph-set-stacked-variable g1 'abc 6))
    (check-equal? (graph-get-stacked-variable g2 'abc (void)) 6)
    (define g3 (graph-push-stacked-variable g2 'abc 7))
    (check-equal? (graph-get-stacked-variable g3 'abc (void)) 7)
    (define g4 (graph-update-stacked-variable g3 'abc add1 (void)))
    (check-equal? (graph-get-stacked-variable g4 'abc (void)) 8)
    (define g5 (graph-pop-stacked-variable g4 'abc))
    (check-equal? (graph-get-stacked-variable g5 'abc (void)) 6)))

;; Querying a graph

(define (all-nodes g)
  (hash-keys (graph-ht-node->attrs g)))

(define all-edges graph-edges)

(define (name-of g node)
  (get-node-attr g node 'name))

(define/memoize (class-of g node)
  (get-node-attr g node 'class))

(define (value-of g node)
  (get-node-attr g node 'value))

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
  #;(eq? 'group (class-of g node))
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
                  (set 'a 'b))
    ))

;; Printing a graph

(define (pr-graph g)
  (displayln "Nodes:")
  (for ([nodeid (sort (set->list (all-nodes g))
                      (λ (id1 id2) (string<? (~a id1) (~a id2))))])
    (printf " ~a ~a\n" (~a nodeid #:min-width 12)
                        (hash-remove (get-node-attrs g nodeid) 'id))
    (for* ([port (node->ports g nodeid)]
           [neighboring-port (port->neighboring-ports g port)])
      (printf "  ~a -- ~a\n" port neighboring-port)))
;  (displayln "Edges:")
;  (define edges (for/list ([e (all-edges g)])
;                  (string-join (sort (stream->list (map ~a e)) string<?))))
;  (for ([edge (sort edges string<?)])
;    (printf " ~a\n" edge))
  )

(define (pr-group g groupid)
  (displayln "Nodes:")
  (for ([nodeid (cons groupid
                      (sort (members-of g groupid)
                            (λ (id1 id2) (string<? (~a id1) (~a id2)))))])
    (printf " ~a ~a\n" (~a nodeid #:min-width 12)
                        (hash-remove (get-node-attrs g nodeid) 'id))
    (for* ([port (node->ports g nodeid)]
           [neighboring-port (port->neighboring-ports g port)])
      (printf "  ~a -- ~a\n" port neighboring-port))))

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
  (match (get-node-attrs g id)
    [(hash-table ((== k) v) _ ...)
     v]
    [_ #f]))

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

;TODO Don't put in a new edge if it's already there
(define (add-edge g edge) ; edge is '((node1 port-label1) (node2 port-label2))
  (match-define `(,port1 ,port2) edge)
  (define edges (graph-edges g))
  (let* ([p->nps (graph-ht-port->neighboring-ports g)]
         [p->nps (hash-update p->nps port1 (λ (st) (set-add st port2)) (set))]
         [p->nps (hash-update p->nps port2 (λ (st) (set-add st port1)) (set))])
    (struct-copy graph g
      [edges (set-add edges (set port1 port2))]
      [ht-port->neighboring-ports p->nps]
      )))

(define (has-edge? g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (set port1 port2))
  (set-member? (graph-edges g) edge*))

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
      [edges (set-remove (graph-edges g) edge*)]
      [ht-port->neighboring-ports p->nps])))

;TODO UT
(define (remove-node g node)
  (let ([g (for/fold ([g g])
                     ([edge (node->incident-edges g node)])
             (remove-edge g edge))])
    (struct-copy graph g
      [ht-node->attrs (hash-remove (graph-ht-node->attrs g) node)])))

(module+ test
  (let* ([g (add-node empty-graph '((class . number) (name . source9)))]
         [g (add-node g '((class . plus)))]
         [g (add-edge g '((source9 output) (plus operand)))])
    (check-true (has-edge? g '((source9 output) (plus operand))))
    (check-true (has-edge? g '((plus operand) (source9 output))))
    (let* ([g (remove-edge g '((plus operand) (source9 output)))])
      (check-false (has-edge? g '((source9 output) (plus operand)))))
    ))

;;; Making a whole graph

(define placeholder
  (let ()
     (struct placeholder [])
     (placeholder)))

(define (placeholder? x) (eq? x placeholder))

;;; Neighbors

(define (port->neighboring-ports g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (hash-ref p->nps port '()))

;TODO refactor
(define (port->neighbors g port)
  (define p->nps (graph-ht-port->neighboring-ports g))
  (for/list ([neighboring-port (in-set (hash-ref p->nps port '()))])
    (match-define (list neighbor _) neighboring-port)
    neighbor))

(define (port->neighbor g port)
  (match (port->neighbors g port)
    ['() (void)]
    [`(,neighbor _ ...) neighbor]))

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

;TODO UT
(define (copy-graph-into-graph g g1)
  (let-values ([(g node-map)
      (for/fold ([g g] [node-map '()])
                ([node (all-nodes g1)])
        (let-values ([(g nodeid) (make-node g (get-node-attrs g1 node))])
          (values g (cons `(,node . ,nodeid) node-map))))])
    (let ([g (for/fold ([g g])
                        ([edge (all-edges g1)])
                (let ([edge (map-edge node-map (set->list edge))])
                  (if (has-edge? g edge) g (add-edge g edge))))])
      (values g (dict-values node-map)))))

