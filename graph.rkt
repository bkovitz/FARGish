#lang racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct "id-set.rkt")
(require racket/pretty)

(provide has-node? make-node add-node get-node-attr get-node-attrs
         make-graph add-tag port->neighbors all-nodes find-nodes-of-class
         check-desiderata pr-graph)

;; A port graph
;(struct graph (elems edges id-set spec) #:transparent)

(struct graph (hm-node->attrs
               hm-port->neighboring-ports
               edges
               id-set
               spec) #:transparent)

(define empty-spec '())
;(define empty-graph (graph #hash() (set) empty-id-set empty-spec))
(define empty-graph (graph #hash() #hash() (set) empty-id-set empty-spec))

#;(define (has-node? g id)
  (hash-has-key? (graph-elems g) id))

(define (has-node? g id)
  (hash-has-key? (graph-hm-node->attrs g) id))

(module+ test
  (check-false (has-node? empty-graph 'plus)))

;; A HACK for now. This should fill in the attrs with defaults from the
;; class definition in the spec. If attrs is just a symbol or number,
;; we should find an appropriate class definition. For now, though, we
;; just hard-code a couple things.
(define (normalize-attrs attrs)
  (match attrs
    [(? symbol? letter)
     (make-immutable-hash `((name . ,letter) (class . letter)))]
    [(hash-table ('name _) _ ...)
     attrs]
    [(hash-table ('class cl) _ ...)
     (hash-set attrs 'name cl)]
    [(? list?)
     (normalize-attrs (make-immutable-hash attrs))]
    [_ (raise-arguments-error 'normalize-attrs "invalid node attributes"
                              "attrs" attrs)]))

(module+ test
  (check-equal? (normalize-attrs 'a)
                #hash((class . letter) (name . a)))
  (check-equal? (normalize-attrs '((class . plus)))
                #hash((class . plus) (name . plus)))
  (check-equal? (normalize-attrs '((class . plus) (name . xyz)))
                #hash((class . plus) (name . xyz))))

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
  (match (graph-hm-node->attrs g)
    [(hash-table ((== id) attrs) _ ...)
     (hash-ref attrs k (void))]
    [_ (void)]))

;;TODO UT
(define (get-node-attrs g id) ;returns void if node not found
  (match (graph-hm-node->attrs g)
    [(hash-table ((== id) attrs) _ ...)
     attrs]
    [_ (void)]))

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

(define (make-graph . items)
  (for/fold ([g empty-graph])
            ([item items])
    (match item
      [(? symbol?) (add-node g item)]
      [`(tag next ,from ,to)
        (let*-values ([(g nextid) (make-node g '((class . next)))]
                      [(g) (add-edge g `((,nextid prev) (,from seq)))]
                      [(g) (add-edge g `((,nextid next) (,to seq)))])
          g)]
      [`(bind ,from ,to)
        (let*-values ([(g bindid) (make-node g '((class . bind)))]
                      [(g) (add-edge g `((,bindid bind-from) (,from bound-to)))]
                      [(g) (add-edge g `((,bindid bind-to) (,to bound-from)))])
          g)]
      [`(placeholder ,name ,class)
        (add-node g `((class . ,class) (name .  ,name) (value . ,placeholder)))]
        )))

;;; Neighbors

(define (port->neighbors g port)
  (define p->nps (graph-hm-port->neighboring-ports g))
  (for/list ([neighboring-port (in-set (hash-ref p->nps port '()))])
    (match-define (list neighbor _) neighboring-port)
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
  (let ([g (make-graph '(placeholder x letter))])
    (check-equal? (get-node-attr g 'x 'class) 'letter)
    (check-pred placeholder? (get-node-attr g 'x 'value))))


;; Tags

(define (add-tag g tag from to)
  (let*-values ([(g bindid) (make-node g '((class . bind)))]
                [(g) (add-edge g `((,bindid bind-from) (,from bound-to)))]
                [(g) (add-edge g `((,bindid bind-to) (,to bound-from)))])
    g))

(module+ test
  (let* ([g (make-graph 'a 'b)]
         [g (add-tag g 'bind 'a 'b)])
    (check-true (has-edge? g '((a bound-to) (bind bind-from))))
    (check-true (has-edge? g '((b bound-from) (bind bind-to))))))

;; Querying the graph

(define (all-nodes g)
  (hash-keys (graph-hm-node->attrs g)))

(define all-edges graph-edges)

(define (find-nodes-of-class g class)
  (for/list ([node (all-nodes g)]
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
    (printf "  ~a\t~a\n" nodeid (hash-remove (get-node-attrs g nodeid) 'id)))
  (displayln "Edges:")
  (define edges (for/list ([e (all-edges g)])
                  (string-join (sort (stream->list (map ~a e)) string<?))))
  (for ([edge (sort edges string<?)])
    (printf "  ~a\n" edge))
  )

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
