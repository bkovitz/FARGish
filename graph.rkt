#lang racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct "id-set.rkt")

(provide has-node? make-node add-node get-node-attr get-node-attrs)

;; A port graph
(struct graph (elems edges id-set spec) #:transparent)

(define empty-spec '())
(define empty-graph (graph #hash() (set) empty-id-set empty-spec))

(define (has-node? g id)
  (hash-has-key? (graph-elems g) id))

(module+ test
  (check-false (has-node? empty-graph 'plus)))

;; A hack for now. This should fill in the attrs with defaults from the
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
                       [elems (hash-set (graph-elems g) id attrs)]
                       [id-set id-set])])
    (values g id)))

(define (get-node-attr g id k) ;returns void if either node or key not found
  (match (graph-elems g)
    [(hash-table ((== id) attrs) _ ...)
     (hash-ref attrs k (void))]
    [_ (void)]))

;TODO UT
(define (get-node-attrs g id) ;returns void if node not found
  (match (graph-elems g)
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

;; Making edges

(define (add-edge g edge)
  (match-define `(,port1 ,port2) edge)
  (define edges (graph-edges g))
  (struct-copy graph g [edges (set-add edges (set port1 port2))]))

(define (has-edge? g edge)
  (match-define `(,port1 ,port2) edge)
  (define edge* (set port1 port2))
  (set-member? (graph-edges g) edge*))

(define (remove-edge g edge)
  (define edge* (apply set edge))
  (struct-copy graph g [edges (set-remove (graph-edges g) edge*)]))

(module+ test
  (let* ([g (add-node empty-graph '((class . number) (name . source9)))]
         [g (add-node g '((class . plus)))]
         [g (add-edge g '((source9 output) (plus operand)))])
    (check-true (has-edge? g '((source9 output) (plus operand))))
    (check-true (has-edge? g '((plus operand) (source9 output))))
    (let* ([g (remove-edge g '((plus operand) (source9 output)))])
      (check-false (has-edge? g '((source9 output) (plus operand)))))
    ))

