#lang debug at-exp racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct
         racket/dict racket/pretty describe rackjure/conditionals
         (except-in "graph.rkt" make-graph))

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

(define (set-lastid g id)
  (struct-copy graph g
               [stacks (let ([st (graph-stacks g)])
                         (dict-set st 'lastid id))]))

(define (current-groupid g)
  (dict-ref (graph-stacks g) 'groupid #f))

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
    [_ (error 'rewrite-item @~a{can't rewrite: @item})]))

(define (get-port g port-spec)
  (match port-spec
    [`((:node . ,args) ,port-label)
      (let-values ([(g id) (make-node/mg g args)])
        (values g `(,id ,port-label)))]
    [`(,name ,port-label)
      (let ([id (look-up-node g name)])
        (if (void? id)
          (error 'get-port @~a{no such node: @name})
          NEXT

(define (do-graph-edits g items)
  (define (recur g items)
    (match items
      ['() g]
      [`((:node . ,args) . ,more)
        (let*-values ([(g id) (make-node g (node-args->attrs args))]
                      [(g) (set-lastid g id)]
                      [(g) (if-let [groupid (current-groupid g)]
                             (add-edge g `((,groupid members) (,id member-of)))
                             g)])
          (recur g more))]
      [`((:edge ,port1 ,port2) . ,more)
        (let*-values ([(g p1) (get-port g port1)]
                      [(g p2) (get-port g port2)]
                      [(edge) `(,p1 ,p2)]
                      [(g) (add-edge g edge)]
                      [(g) (set-lastid g edge)])
          (recur g more))]
      [`(,item . ,more) ;didn't recognize it, rewrite it
        (recur g (cons (rewrite-item g item) more))]
      ))
  (recur g items))

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
  (test-case "placeholder"
    (let ([g (make-graph '(placeholder letter x))])
      (check-equal? (get-node-attr g 'x 'class) 'letter)
      (check-pred placeholder? (get-node-attr g 'x 'value))))
  (test-case ":edge"
    (let ([g (make-graph '(:edge ((:node letter a) out)
                                  ((:node letter b) in)))])
      (check-equal? (all-edges g) (set (set '(a out) '(b in))))))
  (let ([g (make-graph '(:node letter a))])
    (pr-graph g)))
