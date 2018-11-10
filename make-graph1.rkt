; make-graph1.rkt -- Version of make-graph for numbo1.rkt and model1.rkt
;
; Graph operations: a little language for making and editing graphs,
; plus some support functions.
;
; This is a HACK. There should only be one make-graph.rkt. The only real
; difference between this file and make-graph.rkt is that this one calls
; make-node and add-node in fargish.rkt.

#lang debug at-exp errortrace racket

;; Data structure for port graphs

(require "wheel.rkt"
         (except-in "graph1.rkt" make-node add-node add-edge
                                 port->neighbor port->neighbors)
         "fargish1.rkt")

(require rackunit racket/struct
         racket/pretty describe)

(provide make-graph do-graph-edits)

(define (current-groupid g)
  (graph-get-var g 'groupid (void)))
  ;(dict-ref (graph-stacks g) 'groupid #f))

(define (push-groupid g groupid)
  (graph-push-and-set-var g 'groupid groupid))

(define (pop-groupid g)
  (graph-pop-var g 'groupid))

(define (set-lastid g id)
  (graph-set-var g 'lastid id))

(define (get-lastid g)
  (graph-get-var g 'lastid))

(define (set-alias g alias)
  (graph-update-var g 'hm-alias->id
    (λ (hm) (hash-set hm alias (get-lastid g)))
    #hash()))

(define (push-aliases g)
  (graph-push-var g 'hm-alias->id))

(define (pop-aliases g)
  (graph-pop-var g 'hm-alias->id))

(define (look-up-node g name)
  (let ([hm-alias->id (graph-get-var g 'hm-alias->id #hash())])
    (hash-ref hm-alias->id name (thunk (if (has-node? g name) name (void))))))

(define (get-port g port-spec)
  (match port-spec
    [`((:node . ,args) ,port-label)
      (let-values ([(g id) (make-node/mg g args)])
        (values g `(,id ,port-label)))]
    [`(,name ,port-label)
      (let ([id (look-up-node g name)])
        (if (void? id)
          (begin
            ;(pr-graph g)
            (error 'get-port @~a{no such node: @name in port-spec: @port-spec}))
          (values g `(,id ,port-label))))]
    [_ (raise-argument-error 'get-port "(node port-label)" port-spec)]))

#;(define (make-node/mg g args)
  (let*-values ([(g id) (make-node g (node-args->attrs args))]
                [(g) (set-lastid g id)]
                [(g) (let ([groupid (current-groupid g)])
                       (if (void? groupid)
                         g
                         (add-edge g `((,groupid members) (,id member-of)))))])
    (values g id)))

(define (make-node/mg g args)
  (let*-values ([(attrs) (node-args->attrs args)]
                [(g id) (let ([groupid (current-groupid g)])
                          (if (void? groupid)
                            (make-node-with-attrs g attrs)
                            (make-node-with-attrs/in g groupid attrs)))]
                [(g) (set-lastid g id)])
    (values g id)))

#;(define (make-node/mg g args)
  (let*-values ([(args-for-make-node override-attrs) (parse-node-args args)]
                [(g id) (let ([groupid (current-groupid g)])
                          (if (void? groupid)
                            ;NEXT Need to override 'name ?
                            (apply make-node g args-for-make-node)
                            (apply make-node/in g groupid args-for-make-node)))]
                [(g) (if override-attrs
                       (union-node-attrs g id override-attrs)
                       g)]
                [(g) (set-lastid g id)])
    (values g id)))

(define (add-node/mg g args)
  (let-values ([(g _) (make-node/mg g args)])
    g))

(define operators (set '+ '- '* '/))

(define (operator? x)
  (set-member? operators x))

#;(define (node-args->attrs args)
  (match args
    [(? hash? args)
     args]
    [`((:attrs ,attrs))
      attrs]
    [`(,class)
      `((class . ,class))]
    [`(,class ,value)
      `((class . ,class) (value . ,value))]
    [`(,class ,value ,name)
      `((class . ,class) (value . ,value) (name . ,name))]
    [_ (raise-argument-error 'node-args->attrs
                             "(:attrs <alist>), (class), or (class value)"
                             args)]))

(define (node-args->attrs args)
  (match args
    [(? hash? args)
     args]
    [`((:attrs ,attrs))
      (make-immutable-hash attrs)]
    [`(,class)
      (hash 'class class)]
    [`(,class ,value)
      (hash 'class class 'value value)]
    [`(,class ,value ,name)
      (hash 'class class 'value value 'name name)]
    [_ (raise-argument-error 'node-args->attrs
                             "(:attrs <alist>), (class), or (class value)"
                             args)]))

#;(define (hash->class-and-value ht)
  (let ([class (hash-ref ht
                         'class
                         (λ ()
                           (raise-arguments-error 'parse-node-args
                           @~a{Specified node attrs without class: @|ht|.})))])
    (hash-ref/sk ht 'value
                    (λ (value) (list class value))
                    (λ () (list class)))))

; Returns two values: args to pass to make-node, hash table of overriding attrs
; or #f if no overrides
#;(define (parse-node-args args)
  args
  (match args
    [(? hash? args)
     (values (hash->class-and-value args)
             (hash-remove* args 'class 'value))]
    [`((:attrs ,attrs))
      (if (hash? attrs)
        (parse-node-args attrs)
        (parse-node-args (make-immutable-hash attrs)))]
    [`(,class)
      (values (list class) #f)]
    [`(,class ,value)
      (values (list class value) #f)]
    [`(,class ,value ,name)
      (values (list class value) (hash 'name name))]
    [_ (raise-argument-error 'parse-node-args
                             @~a{Invalid :node arguments: @|args|.})]))

(define (parse-group-elems elems) ; returns attrs, true elems  (two values)
  (define (recur elems attrs true-elems)
    (match elems
      ['() (values attrs (reverse true-elems))]
      [`((:name ,name) . ,more)
        (recur more (hash-set attrs 'name name) true-elems)]
      [`((:class ,class) . ,more)
        (recur more (hash-set attrs 'class class) true-elems)]
      [`(,elem . ,more)
        (recur more attrs (cons elem true-elems))]))
  (recur elems #hash((class . group) (group? . #t)) '()))

(define (rewrite-item g item)
  (match item
    [(? operator?) `(:node (:attrs ((class . operator)
                                    (name . ,item)
                                    (value . ,item))))]
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
    [`(add-tag ,tag . ,nodes)
      (apply add-tag g tag nodes)]
;    [`(bind ,a ,b)
;      `(:edgenode bind ,a ,b (bound-to bind-from) (bind-to bound-from))]
;    [`(succ ,a ,b . ,more)
;      (let loop ([a a] [b b] [more more])
;        `(:begin
;           (:edgenode succ ,a ,b (succ-to succ-from) (succ-to succ-from))
;           ,@(if (null? more) '() (list (loop b (car more) (cdr more))))))]
    [`(boost-salience ,node)
      `(:update-attr ,node salience ,(λ (old) (+ old 1.0)) 0.0)]
    [`(reduce-salience ,node)
      `(:update-attr ,node salience ,(λ (old) (* old 0.5)) 0.0)]
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
      [`((:remove-node ,node) . ,more)
        (recur (remove-node g node) more)]
      [`((:let ([,nm ,thing] ...) ,body ...) . ,more)
        (for/fold ([g (push-aliases g)]
                   #:result (recur (pop-aliases (do-graph-edits g body)) more))
                  ([n nm] [t thing])
          (let ([g (do-graph-edits g (list t))])
            (set-alias g n)))]
      [`((:begin . ,xs) . ,more)
        (recur (recur g xs) more)]
      [`((:group . ,xs) . ,more)
        (let*-values ([(attrs xs) (parse-group-elems xs)]
                      [(g groupid) (make-node/mg g attrs)]
                      [(g) (push-groupid g groupid)]
                      [(g) (recur g xs)]
                      [(g) (pop-groupid g)]
                      [(g) (set-lastid g groupid)])

;         (let*-values ([(g groupid) (make-node/mg g
;                                      `((:attrs ((class . group)
;                                                 (name . ,name)))))]
;                       [(g) (push-groupid g groupid)]
;                       [(g) (recur g xs)]
;                       [(g) (pop-groupid g)]
;                       [(g) (set-lastid g groupid)])
           (recur g more))]
      [`((:update-attr ,node ,k ,f ,failure-result) . ,more)
        (let ([g (update-node-attr g node k f failure-result)])
          (recur g more))]
      [`(,item . ,more) ;didn't recognize it, rewrite it
        (recur g (cons (rewrite-item g item) more))]
      ))
  (recur g items))

;HACK
(define unit-test-spec
  (farg-model-spec
    (nodeclass letter)
    (nodeclass number)
    (nodeclass operator)
    (make-nodeclass placeholder)
    (nodeclass group)
    (nodeclass bind)
    (nodeclass succ)
    ))

(define (make-graph . items)
  (do-graph-edits (make-empty-graph unit-test-spec) items))

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
      (check-equal? (list->set (all-edges g)) (set (set '(a out) '(b in))))))
  (test-case ":edge between refs"
    (let ([g (make-graph 'a 'b '(:edge (a out) (b in)))])
      (check-equal? (list->set (all-edges g)) (set (set '(a out) '(b in))))))
  (test-case ":edge between numbers"
    (let ([g (make-graph 4 '+ '(:edge (4 result) (+ operands)))])
      (check-equal? (list->set (all-edges g))
                    (set (set '(4 result) '(+ operands))))))
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
      (check-equal? (list->set (all-edges g)) (set (set '(a in) '(b out))))))
  (test-case ":begin"
    (let ([g (make-graph '(:begin a b))])
      (check-equal? (list->set (all-nodes g)) (set 'a 'b))
      (check-equal? (class-of g 'a) 'letter)
      (check-equal? (class-of g 'b) 'letter)))
  (test-case ":group"
    (let ([g (make-graph '(:group (:name outer) a b
                                  (:group (:name inner) c d) e) 'f)])
      (check-equal? (list->set (all-nodes g))
                    (set 'a 'b 'c 'd 'e 'f 'inner 'outer))
      (check-equal? (list->set (members-of g 'outer))
                    (set 'inner 'a 'b 'e))
      (check-equal? (list->set (members-of g 'inner))
                    (set 'c 'd))))
;  (test-case "bind"
;    (let ([g (make-graph 'a 'b '(bind a b))])
;      (check-true (bound-to? g 'a 'b))
;      (check-true (bound-from? g 'b 'a))))
;  (test-case "succ"
;    (let ([g (make-graph 'a 'b 'c '(succ a b c))])
;      (check-true (succ? g 'a 'b))
;      (check-true (succ? g 'b 'c))))
  )

;; Tags

(module+ test
  (test-case "placeholder class, name, and value"
    (let ([g (make-graph '(placeholder letter x))])
        (check-equal? (get-node-attr g 'x 'class) 'letter)
        (check-pred placeholder? (get-node-attr g 'x 'value)))))

;;; Desiderata
;
;; Eventually this should get a third argument: the node with the desiderata,
;; or the group in which to search.
;(define (check-desiderata g)
;  (define nodes-of-interest (find-nodes-of-class g 'bind))
;  (for/hash ([node nodes-of-interest])
;    (values node (port->neighbors g `(,node basis)))))
;
;(module+ test
;  (test-case "check-desiderata"
;    (let* ([g (make-graph 'a 'b 'x '(bind a b))]
;           [desiderata-status (check-desiderata g)])
;      (check-equal? desiderata-status #hash((bind . ())))
;      (let* ([g (add-edge g '((bind basis) (x basis-of)))]
;           [desiderata-status (check-desiderata g)])
;        (check-equal? desiderata-status #hash((bind . (x))))))))
