#lang debug at-exp errortrace racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct
         racket/dict racket/pretty describe "wheel.rkt" "graph.rkt")

(provide make-graph do-graph-edits tag-of check-desiderata tags-of class-is-a?
         node-is-a? has-tag? tag? taggees-of)

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

(define operators (set '+ '- '* '/))

(define (operator? x)
  (set-member? operators x))

(define (node-args->attrs args)
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
      `(:let ([:tag (:node ,(tag->attrs tag))])
         ,@(for/list ([node nodes])
             `(:edge (:tag tagged) (,node tags))))]
    [`(add-tag2 ,tag ,node1 ,node2) ;asymmetric tag
      `(:let ([:tag (:node ,(tag->attrs tag))])
         (:edge (:tag tagged-a) (,node1 tags))
         (:edge (:tag tagged-b) (,node2 tags)))]
    [`(bind ,a ,b)
      `(:edgenode bind ,a ,b (bound-to bind-from) (bind-to bound-from))]
    [`(succ ,a ,b . ,more)
      (let loop ([a a] [b b] [more more])
        `(:begin
           (:edgenode succ ,a ,b (succ-to succ-from) (succ-to succ-from))
           ,@(if (null? more) '() (list (loop b (car more) (cdr more))))))]
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
  (test-case "bind"
    (let ([g (make-graph 'a 'b '(bind a b))])
      (check-true (bound-to? g 'a 'b))
      (check-true (bound-from? g 'b 'a))))
  (test-case "succ"
    (let ([g (make-graph 'a 'b 'c '(succ a b c))])
      (check-true (succ? g 'a 'b))
      (check-true (succ? g 'b 'c)))))

;; Tags

(define (tag->attrs tag)
  (match tag
    [`(,class . ,args)
     `(:attrs ((tag? . #t) (class . ,class) (value . ,tag)))]
    [class #:when (not (list? class))
     `(:attrs ((tag? . #t) (class . ,class) (value . ,tag)))]
    [_ (raise-argument-error 'tag->attrs "invalid tag" "tag" tag)]))

(define (tag? g node)
  (node-attr? g 'tag? node))

;; Symmetric tag  TODO Obsolete? Just do add-tag in do-graph-edits?
#;(define (add-tag g tag . nodes)
  (when (empty? nodes)
    (raise-argument-error 'add-tag "must tag at least one node" "nodes" nodes))
  (do-graph-edits g
    `((:let ([:tag (:node ,(tag->attrs tag))])
         ,@(for/list ([node nodes])
             `(:edge (:tag tagged) (,node tags)))))))

(define (class-is-a? g ancestor class)
  (equal? ancestor class)) ;TODO Inheritance

(define (node-is-a? g class node)
  (class-is-a? g class (class-of g node)))

;TODO Match placeholders in tagspec?
;Returns the first matching tag or #f if none
(define (has-tag? g tagspec node)
  (define is?
    (match tagspec
      [`(,tagclass . ,tagargs)
       (λ (tagnode) (and (class-is-a? g (class-of g tagnode) tagclass)
                         (equal? tagargs (safe-cdr (value-of g tagnode)))))]
      [tagclass #:when (not (list? tagclass))
       (λ (tagnode) (class-is-a? g (class-of g tagnode) tagclass))]
      [_ (raise-argument-error 'has-tag?
                               "tagspec must be (tagclass . args) or tagclass"
                               "tagspec" tagspec)]))
  (for/or ([tagnode (tags-of g node)])
    (if (is? tagnode) tagnode #f)))

;  (define attrs #R (cadr (tag->attrs tag)))
;  (define class (dict-ref attrs 'class))
;  (define value (dict-ref attrs 'value (void)))
;  (define (is? tagnode)
;    (and (equal? (class-of g tagnode) class)
;         (or (void? value) (equal? (value-of g tagnode) #R value))))
;  (for/or ([tagnode (port->neighbors g `(,node tags))])
;    (is? tagnode)))

(module+ test
  (test-case "add-tag"
    (let* ([g (make-graph 'a 'b 'c '(add-tag near a b))])
      (check-not-false (has-tag? g 'near 'a))
      (check-not-false (has-tag? g 'near 'b))
      (check-false (has-tag? g 'near 'c)))
    (let* ([g (make-graph 'a 'b '(add-tag (needs-neighbor source) a)
                                '(add-tag (needs-neighbor result) b))])
      (check-not-false (has-tag? g 'needs-neighbor 'a))
      (check-not-false (has-tag? g '(needs-neighbor source) 'a))
      (check-not-false (has-tag? g 'needs-neighbor 'b))
      (check-false (has-tag? g '(needs-neighbor source) 'b))))
  (test-case "add-tag via rewrite"
    (let* ([g (make-graph 'a 'b 'c '(add-tag near a b))])
      (check-not-false (has-tag? g 'near 'a))
      (check-not-false (has-tag? g 'near 'b))
      (check-false (has-tag? g 'near 'c)))
    (let* ([g (make-graph 'a 'b
                          '(add-tag (needs-neighbor source) a)
                          '(add-tag (needs-neighbor result) b))])
      (check-not-false (has-tag? g 'needs-neighbor 'a))
      (check-not-false (has-tag? g '(needs-neighbor source) 'a))
      (check-not-false (has-tag? g 'needs-neighbor 'b))
      (check-false (has-tag? g '(needs-neighbor source) 'b)))
    ))

;TODO This is way too model- and tag-specific
(define (tag-of tag g node1 node2)
  (match tag
    ['succ (for*/first ([tag1 (port->neighbors g `(,node1 succ-to))]
                        [tag2 (port->neighbors g `(,node2 succ-from))]
                        #:when (equal? tag1 tag2))
             tag)]
    [_ (raise-arguments-error 'tag-of "unknown tag" "tag" tag)]))

(define (tags-of g node)
  (port->neighbors g `(,node tags)))

;TODO This is way too model- and tag-specific
(define (taggees-of g tagnode)
  (port->neighbors g `(,tagnode tagged)))

#;(define (add-tag g tag from to)
  (let*-values ([(g bindid) (make-node g '((class . bind)))]
                [(g) (add-edge g `((,bindid bind-from) (,from bound-to)))]
                [(g) (add-edge g `((,bindid bind-to) (,to bound-from)))])
    g))

#;(module+ test
  (let* ([g (make-graph 'a 'b)]
         [g (add-tag g 'bind 'a 'b)])
    (check-true (has-edge? g '((a bound-to) (bind bind-from))))
    (check-true (has-edge? g '((b bound-from) (bind bind-to))))))

(module+ test
  (test-case "tag-of"
    (let ([g (make-graph 'a 'b 'c '(succ a b))])
      (check-equal? (tag-of 'succ g 'a 'b) 'succ))))

(module+ test
  (test-case "placeholder class, name, and value"
    (let ([g (make-graph '(placeholder letter x))])
        (check-equal? (get-node-attr g 'x 'class) 'letter)
        (check-pred placeholder? (get-node-attr g 'x 'value)))))

;; Desiderata

; Eventually this should get a third argument: the node with the desiderata,
; or the group in which to search.
(define (check-desiderata g)
  (define nodes-of-interest (find-nodes-of-class g 'bind))
  (for/hash ([node nodes-of-interest])
    (values node (port->neighbors g `(,node basis)))))

(module+ test
  (test-case "check-desiderata"
    (let* ([g (make-graph 'a 'b 'x '(bind a b))]
           [desiderata-status (check-desiderata g)])
      (check-equal? desiderata-status #hash((bind . ())))
      (let* ([g (add-edge g '((bind basis) (x basis-of)))]
           [desiderata-status (check-desiderata g)])
        (check-equal? desiderata-status #hash((bind . (x))))))))
