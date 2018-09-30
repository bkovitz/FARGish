#lang debug at-exp racket

;TODO Remove make-graph and associated functions from this file.

;; Data structure for port graphs

(require rackunit racket/generic racket/struct "id-set.rkt"
         racket/dict racket/pretty describe mischief/memoize) 

(provide make-node add-node add-edge get-node-attr get-node-attrs set-node-attr
         port->neighbors port->neighboring-ports all-nodes
         find-nodes-of-class value-of port->neighbor bound-from-ctx-to-ctx?
         check-desiderata pr-graph pr-group members-of member-of?
         nodes-of-class-in class-of bind members-of member-of next-to? bound-to?
         bound-from? succ? has-node? node->neighbors node->ports
         node->incident-edges port->incident-edges has-edge? all-edges
         empty-graph placeholder placeholder? group? remove-node
         graph-set-stacked-variable graph-get-stacked-variable
         graph-push-stacked-variable graph-pop-stacked-variable
         graph-update-stacked-variable node-attr?
         (struct-out graph))

;; A port graph

(struct graph (hm-node->attrs
               hm-port->neighboring-ports
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
  (hash-keys (graph-hm-node->attrs g)))

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
  (eq? 'group (class-of g node)))

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
  (let* ([g (make-graph 'a 'b)]
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
  (hash-has-key? (graph-hm-node->attrs g) id))

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
    [(hash-table ('value v) _ ...)
     v]
    [(hash-table ('class c) _ ...)
     c]))

(define (default-name attrs) ; N.b.: ignores 'name key even if it's there
  (match attrs
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
                #hash((class . plus) (value . xyz) (name . plusxyz))))

;; Making nodes

(define (make-node g attrs) ;returns g* id  (two values)
  (let*-values ([(attrs) (normalize-attrs attrs)]
                [(name) (hash-ref attrs 'name)]
                [(id-set id) (gen-id (graph-id-set g) name)]
                [(attrs) (hash-set attrs 'id id)]
                [(g) (struct-copy graph g
                       [hm-node->attrs
                         (hash-set (graph-hm-node->attrs g) id attrs)]
                       [id-set id-set])])
    (values g id)))

(define (get-node-attr g id k) ;returns void if either node or key not found
  (let ([hm (get-node-attrs g id)])
    (if (void? hm)
      (void)
      (hash-ref (get-node-attrs g id) k (void)))))
;  (match (graph-hm-node->attrs g)
;    [(hash-table ((== id) attrs) _ ...)
;     (hash-ref attrs k (void))]
;    [_ (void)]))

;;TODO UT
;; Returns value of id's attribute k, or #f if either node or key not found
(define (node-attr? g id k)
  (match (get-node-attrs g id)
    [(hash-table ((== k) v) _ ...)
     v]
    [_ #f]))

;;TODO UT
(define (get-node-attrs g id) ;returns void if node not found
  (match (graph-hm-node->attrs g)
    [(hash-table ((== id) attrs) _ ...)
     attrs]
    [_ (void)]))

;TODO UT
(define (set-node-attr g node k v)
  (if (has-node? g node)
    (let ([attrs (get-node-attrs g node)])
      (struct-copy graph g
        [hm-node->attrs
          (hash-set (graph-hm-node->attrs g) node (hash-set attrs k v))]))
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
  (let* ([p->nps (graph-hm-port->neighboring-ports g)]
         [p->nps (hash-update p->nps port1 (λ (st) (set-add st port2)) (set))]
         [p->nps (hash-update p->nps port2 (λ (st) (set-add st port1)) (set))])
    (struct-copy graph g
      [edges (set-add edges (set port1 port2))]
      [hm-port->neighboring-ports p->nps]
      )))

(define (has-edge? g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (set port1 port2))
  (set-member? (graph-edges g) edge*))

(define (remove-edge g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (apply set edge))
  (let* ([p->nps (graph-hm-port->neighboring-ports g)]
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
      [hm-port->neighboring-ports p->nps])))

;TODO UT
(define (remove-node g node)
  (let ([g (for/fold ([g g])
                     ([edge (node->incident-edges g node)])
             (remove-edge g edge))])
    (struct-copy graph g
      [hm-node->attrs (hash-remove (graph-hm-node->attrs g) node)])))

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

;TODO OAOO
(define (bind g from-node to-node)
  (let*-values ([(g bindid) (make-node g '((class . bind)))]
                [(g) (add-edge g `((,bindid bind-from)
                                   (,from-node bound-to)))]
                [(g) (add-edge g `((,bindid bind-to)
                                   (,to-node bound-from)))])
    g))

;; Another way to build/edit a graph

(define (rewrite-item g item)
  (match item
    [(? symbol?) `(:node letter ,item)]
    [(? number?) `(:node number ,item)]
    [`(placeholder)
      (rewrite-item g `(placeholder ,placeholder placeholder))]
    [`(placeholder ,class)
      (rewrite-item g `(placeholder ,class placeholder))]
    [`(placeholder ,class ,name)
      `(:node (:attrs ((name . ,name)
                       (class . ,class)
                       (value . ,placeholder))))]
    [`(next ,a ,b . ,more)
      (let loop ([a a] [b b] [more more])
        (define n (gensym 'next))
        `(:make
           (:define ,n (:node next))
           (:edge (,a seq) (,n prev))
           (:edge (,n next) (,b seq))
           ,@(if (null? more) '() (list (loop b (car more) (cdr more))))))]
    [`(bind ,a ,b)
      (define sym (gensym 'bind))
      `(:make
         (:define ,sym (:node bind))
         (:edge (,a bound-to) (,sym bind-from))
         (:edge (,sym bind-to) (,b bound-from)))]
    [`(copy-node ,node ,ctx)
      (define class (class-of g node))
      (define sym (gensym class))
      `(:define ,sym (:node ,class))]
    [`(succ ,a ,b . ,more)
      (let loop ([a a] [b b] [more more])
        (define sym (gensym 'succ))
        `(:make
           (:define ,sym (:node succ))
           (:edge (,a succ-to) (,sym succ-from))
           (:edge (,sym succ-to) (,b succ-from))
           ,@(if (null? more) '() (list (loop b (car more) (cdr more))))))]
;    [`(numbo-ws (bricks . ,bricks) (target ,target))
;      (define ws (gensym 'ws))
;      (define target (gensym 'target))
;      `(:make
;         (:define ,ws (:node ws))
;         ,(let loop ([brick bricks])
;            (define b (gensym 'brick))
;            (
    [_ (error 'rewrite-item @~a{can't rewrite: @item})]))

(define (node-args->attrs args)
  (match args
    [`((:attrs ,attrs))
      attrs]
    [`(,class)
      `((class . ,class))]
    [`(,class ,value)
      `((class . ,class) (value . ,value))]
    [_ (raise-argument-error 'node-args->attrs
                             "(:attrs <alist>), (class), or (class value)"
                             args)]))

(define (look-up-node g d-alias->id name)
  (dict-ref d-alias->id name (λ () (if (has-node? g name) name (void)))))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (graph-edit-make-node g d-alias->id groupid args)
  (let*-values ([(g id) (make-node g (node-args->attrs args))]
                [(d-alias->id) (let ([v (value-of g id)])
                                (if (void? v)
                                  d-alias->id
                                  (dict-set d-alias->id (name-of g id) id)))]
                [(g) (if (void? groupid)
                       g
                       (add-edge g `((,groupid members) (,id member-of))))])
    (values g d-alias->id id)))

(define (get-port g d-alias->id groupid port-spec)
  (match port-spec
    [`((:node . ,args) ,port-label)
      (let*-values ([(g d-alias->id id)
                       (graph-edit-make-node g d-alias->id groupid args)])
        (values g d-alias->id `(,id ,port-label)))]
    [`(,name ,port-label)
      (let ([id (look-up-node g d-alias->id name)])
        (if (void? id)
          (error 'get-port @~a{no such node: @name})
          (values g d-alias->id `(,id ,port-label))))]
    [_ (raise-argument-error 'get-port "(node port-label)" port-spec)]))


(define (do-graph-edits g items)
  (define (recur g d-alias->id groupid last-id items)
    (match items
      ['() (values g d-alias->id last-id)]
      [`((:node . ,args) . ,more)
        (let-values ([(g d-alias->id id)
                        (graph-edit-make-node g d-alias->id groupid args)])
          (recur g d-alias->id groupid id more))]
      [`((:find-node ,name) . ,more)
        (let ([id (look-up-node g d-alias->id name)])
          (if (void? id)
            (error @~a{no such node: @id})
            (recur g d-alias->id groupid id more)))]
      [`((:edge ,port1 ,port2) . ,more)
        (let*-values ([(g d-alias->id port1)
                         (get-port g d-alias->id groupid port1)]
                      [(g d-alias->id port2)
                         (get-port g d-alias->id groupid port2)]
                      [(edge) `(,port1 ,port2)]
                      [(g) (add-edge g edge)])
          (recur g d-alias->id groupid edge more))]
      [`((:define ,name ,item) . ,more)
        (let*-values ([(g d-alias->id itemid)
                         (recur g d-alias->id groupid last-id (list item))]
                      [(d-alias->id) (dict-set d-alias->id name itemid)])
          (recur g d-alias->id groupid itemid more))]
      [`((:make . ,makes) . ,more)
        (let-values ([(g d-alias->id itemid)
                        (recur g d-alias->id groupid last-id makes)])
          (recur g d-alias->id groupid itemid more))]
      [`((:group ,name . ,members) . ,more)
        (let*-values ([(g d-alias->id new-groupid)
                         (graph-edit-make-node g d-alias->id groupid
                                               `((:attrs ((class . group)
                                                          (name . ,name)))))]
                      [(g d-alias->id last-id)
                         (recur g d-alias->id new-groupid new-groupid members)])
          (recur g d-alias->id groupid new-groupid more))]
      [`(,item . ,more) ;didn't recognize it, rewrite it
        (recur g d-alias->id groupid last-id
               (cons (rewrite-item g item) more))]))
  (let-values ([(g d id) (recur g '() (void) (void) items)])
    g))

(define (make-graph . items)
  (do-graph-edits empty-graph items))

(module+ test
  (test-case ":node"
    (let ([g (make-graph '(:node letter a) '(:node letter b))])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (class-of g 'a) 'letter)
      (check-equal? (class-of g 'b) 'letter)))
  (test-case "a b"
    (let ([g (make-graph 'a 'b)])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (class-of g 'a) 'letter)
      (check-equal? (class-of g 'b) 'letter)))
  (test-case ":edge"
    (let ([g (make-graph '(:edge ((:node letter a) out)
                                  ((:node letter b) in)))])
      (check-equal? (all-edges g) (set (set '(a out) '(b in))))))
  (test-case ":edge betw refs"
    (let ([g (make-graph 'a 'b '(:edge (a out) (b in)))])
      (check-equal? (all-edges g) (set (set '(a out) '(b in))))))
  (test-case "make-graph with placeholders"
    (let ([g (make-graph 'a '(placeholder letter) 'c '(placeholder letter)
                         '(placeholder letter X) '(placeholder))])
      (check-equal? (class-of g 'placeholder) 'letter)
      (check-equal? (value-of g 'placeholder) placeholder)
      (check-equal? (class-of g 'placeholder2) 'letter)
      (check-equal? (value-of g 'placeholder2) placeholder)
      (check-equal? (class-of g 'X) 'letter)
      (check-equal? (value-of g 'X) placeholder)
      (check-equal? (class-of g 'placeholder3) placeholder)
      (check-equal? (value-of g 'placeholder3) placeholder)))
  (test-case ":define"
    (let ([g (make-graph '(:define :a (:node letter b))  ; :a is b
                         '(:node letter a)
                         '(:edge (:a out) (a in)))])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (all-edges g) (set (set '(a in) '(b out))))))
  (test-case "make-graph next"
    (let ([g (make-graph '(:make a b c (next a b c)))])
      (check-equal? (all-edges g) (set
                                    (set '(next next) '(b seq))
                                    (set '(next2 prev) '(b seq))
                                    (set '(c seq) '(next2 next))
                                    (set '(a seq) '(next prev))))))
  (test-case ":group"
    (let ([g (make-graph 'a '(placeholder letter X) 'c '(next a X c)
                         '(:group archetype a b c (next a b c)))])
      (check-equal? (list->set (members-of g 'archetype))
                    (set 'a2 'b 'c2 'next3 'next4))
      (check-true (next-to? g 'a 'X 'c))
      (check-true (next-to? g 'a2 'b 'c2))))
  (test-case "bind"
    (let ([g (make-graph 'a 'b '(bind a b))])
      (check-true (bound-to? g 'a 'b))
      (check-true (bound-from? g 'b 'a))))
  (test-case "succ"
    (let ([g (make-graph 'a 'b 'c '(succ a b c))])
      (check-true (succ? g 'a 'b))
      (check-true (succ? g 'b 'c))))
  )

;;; Neighbors

(define (port->neighboring-ports g port)
  (define p->nps (graph-hm-port->neighboring-ports g))
  (hash-ref p->nps port '()))

;TODO refactor
(define (port->neighbors g port)
  (define p->nps (graph-hm-port->neighboring-ports g))
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
  (for/list ([port (hash-keys (graph-hm-port->neighboring-ports g))]
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
  (let* ([g (make-graph 'a 'b 'c)]
         [g (add-edge g '((a out) (b in)))]
         [g (add-edge g '((a out) (c in)))])
    (check-equal? (list->set (port->neighbors g '(a out)))
                  (set 'b 'c))
    (let* ([g (remove-edge g '((b in) (a out)))])
      (check-equal? (port->neighbors g '(a out)) '(c)))
    )
  (let ([g (make-graph '(placeholder letter x))])
    (check-equal? (get-node-attr g 'x 'class) 'letter)
    (check-pred placeholder? (get-node-attr g 'x 'value))))

;; Desiderata

; Eventually this should get a third argument: the node with the desiderata,
; or the group in which to search.
(define (check-desiderata g)
  (define nodes-of-interest (find-nodes-of-class g 'bind))
  (for/hash ([node nodes-of-interest])
    (values node (port->neighbors g `(,node basis)))))

(module+ test
  (let* ([g (make-graph 'a 'b 'x '(bind a b))]
         [desiderata-status (check-desiderata g)])
    (check-equal? desiderata-status #hash((bind . ())))
    (let* ([g (add-edge g '((bind basis) (x basis-of)))]
         [desiderata-status (check-desiderata g)])
      (check-equal? desiderata-status #hash((bind . (x)))))
    ))
