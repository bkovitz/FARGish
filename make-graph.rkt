#lang debug at-exp racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct
         racket/dict racket/pretty describe
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

(define (current-groupid g)
  (graph-get-stacked-variable g 'groupid (void)))
  ;(dict-ref (graph-stacks g) 'groupid #f))

(define (push-groupid g groupid)
  (graph-push-stacked-variable g 'groupid groupid))

(define (pop-groupid g)
  (graph-pop-stacked-variable g 'groupid))

#;(define (set-alias g alias)
  (let ([hm-alias->id (dict-ref (graph-stacks g) 'hm-alias->id #hash())])
    (graph-set-stacked-variable g 'hm-alias->id hm-alias->id)))

#;(define (set-lastid g id)
  (struct-copy graph g
               [stacks (let ([st (graph-stacks g)])
                         (dict-set st 'lastid id))]))

(define (set-lastid g id)
  (graph-set-stacked-variable g 'lastid id))

(define (get-lastid g)
  (graph-get-stacked-variable g 'lastid))

(define (set-alias g alias)
  (graph-update-stacked-variable g 'hm-alias->id
    (λ (hm) (hash-set hm alias (get-lastid g)))
    #hash()))

(define (push-aliases g)
  (graph-push-stacked-variable g 'hm-alias->id))

(define (pop-aliases g)
  (graph-pop-stacked-variable g 'hm-alias->id))

(define (look-up-node g name)
  (let ([hm-alias->id (graph-get-stacked-variable g 'hm-alias->id #hash())])
    (hash-ref hm-alias->id name (thunk (if (has-node? g name) name (void))))))

(define (get-port g port-spec)
  (match port-spec
    [`((:node . ,args) ,port-label)
      (let-values ([(g id) (make-node/mg g args)])
        (values g `(,id ,port-label)))]
    [`(,name ,port-label)
      (let ([id (look-up-node g name)])
        (if (void? id)
          (error 'get-port @~a{no such node: @name})
          (values g `(,id ,port-label))))]
    [_ (raise-argument-error 'get-port "(node port-label)" port-spec)]))

(define (make-node/mg g args)
  (let*-values ([(g id) (make-node g (node-args->attrs args))]
                [(g) (set-lastid g id)]
                [(g) (let ([groupid (current-groupid g)])
                       (if (void? groupid)
                         g
                         (add-edge g `((,groupid members) (,id member-of)))))])
    (values g id)))

(define (add-node/mg g args)
  (let-values ([(g _) (make-node/mg g args)])
    g))

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
    [`(copy-node ,node ,ctx)
      `(:node ,(class-of g node))]
    [`(bind ,a ,b)
      `(:edgenode bind ,a ,b (bound-to bind-from) (bind-to bound-from))]
    [`(succ ,a ,b . ,more)
      (let loop ([a a] [b b] [more more])
        `(:begin
           (:edgenode succ ,a ,b (succ-to succ-from) (succ-to succ-from))
           ,@(if (null? more) '() (list (loop b (car more) (cdr more))))))]
    [_ (error 'rewrite-item @~a{can't rewrite: @item})]))

(define (do-graph-edits g items)
  (define (recur g items)
    (match items
      ['() g]
      [`((:node . ,args) . ,more)
        (recur (add-node/mg g args) more)]
      [`((:edge ,port1 ,port2) . ,more)
        (let*-values ([(g p1) (get-port g port1)]
                      [(g p2) (get-port g port2)]
                      [(edge) `(,p1 ,p2)]
                      [(g) (add-edge g edge)]
                      [(g) (set-lastid g edge)])
          (recur g more))]
      [`((:edgenode ,class ,from-node ,to-node
                    (,from-port-label ,edge-port-label1)
                    (,edge-port-label2 ,to-port-label)) . ,more)
        (let*-values ([(g id) (make-node/mg g `((:attrs ((class . ,class)))))]
                      [(g) (add-edge g `((,from-node ,from-port-label)
                                         (,id ,edge-port-label1)))]
                      [(g) (add-edge g `((,id ,edge-port-label2)
                                         (,to-node ,to-port-label)))]
                      [(g) (set-lastid g id)])
          (recur g more))]
      [`((:let ([,nm ,thing] ...) ,body ...) . ,more)
        (for/fold ([g (push-aliases g)]
                   #:result (recur (pop-aliases (do-graph-edits g body)) more))
                  ([n nm] [t thing])
          (let ([g (do-graph-edits g (list t))])
            (set-alias g n)))]
      [`((:begin . ,xs) . ,more)
        (recur (recur g xs) more)]
      [`((:group ,name . ,xs) . ,more)
         (let*-values ([(g groupid) (make-node/mg g
                                      `((:attrs ((class . group)
                                                 (name . ,name)))))]
                       [(g) (push-groupid g groupid)]
                       [(g) (recur g xs)]
                       [(g) (pop-groupid g)]
                       [(g) (set-lastid g groupid)])
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
  (test-case ":let"
    (let ([g (make-graph '(:let ([:a (:node letter b)])  ; :a is b
                             (:node letter a)
                             (:edge (:a out) (a in))))])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (all-edges g) (set (set '(a in) '(b out))))))
  (test-case ":begin"
    (let ([g (make-graph '(:begin a b))])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (class-of g 'a) 'letter)
      (check-equal? (class-of g 'b) 'letter)))
  (test-case ":group"
    (let ([g (make-graph '(:group outer a b (:group inner c d) e) 'f)])
      (check-equal? (set 'a 'b 'c 'd 'e 'f 'inner 'outer)
                    (list->set (all-nodes g)))
      (check-equal? (set 'inner 'a 'b 'e)
                    (list->set (members-of g 'outer)))
      (check-equal? (set 'c 'd)
                    (list->set (members-of g 'inner)))))
  (test-case "bind"
    (let ([g (make-graph 'a 'b '(bind a b))])
      (check-true (bound-to? g 'a 'b))
      (check-true (bound-from? g 'b 'a))))
  (test-case "succ"
    (let ([g (make-graph 'a 'b 'c '(succ a b c))])
      (check-true (succ? g 'a 'b))
      (check-true (succ? g 'b 'c))))

  (let ([g (make-graph '(:node letter a))])
    (pr-graph g)))
