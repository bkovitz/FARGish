; numbo0 -- A "hacked" Numbo that does most things in non-FARG-like ways

#lang debug at-exp racket

;TODO
; bind/complete archetype in one step  DONE
; tag scout
; slipnet search  DONE
; large number or diff attracts mult
; smaller target attracts subtraction
; done?  DONE
; printing the result  DONE

(require rackunit (except-in data/collection cartesian-product range)
         racket/dict racket/generic racket/pretty
         racket/hash profile describe
         "wheel.rkt" "xsusp3.rkt" "graph.rkt" "make-graph.rkt")
(require (only-in racket/base (log logarithm)))
(require (only-in racket/list range cartesian-product))

(provide (all-defined-out))

;; Global constants

(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 20)
(define support-decay-rate 0.7)

;; Making a workspace

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g '((class . numbo-ws)))]
                [(g) (for/fold ([g g])
                               ([brick bricks])
                       (let*-values ([(g brickid) (make-node g brick)]
                                     [(g) (add-edge g `((,ws bricks)
                                                        (,brickid source)))]
                                     [(g) (add-edge g `((,ws members)
                                                        (,brickid
                                                          member-of)))])
                         g))]
                [(g targetid) (make-node g target)]
                [(g) (add-edge g `((,ws target) (,targetid result)))]
                [(g) (add-edge g `((,ws members) (,targetid member-of)))])
    g))

#;(module+ test
  (test-case "numbo-ws"
    (let ([g (make-numbo-ws (make-graph) '(4 5 6) 15)])
      #f
      ;TODO
      )))

;; Searching the numbo-ws

(define (done? g)
  (define (has-source? node)
    (case (class-of g node)
      [(number)
       (define sources (port->neighbors g `(,node source)))
       (if (empty? sources)
         #f
         (for/and ([source sources])
           (has-source? source)))]
      [(operator)
       (define operands (port->neighbors g `(,node operands)))
       (if (empty? operands)
         #f
         (for/and ([operand operands])
           (has-source? operand)))]
      [(numbo-ws)
       #t]))
  ;TODO? throw error if no match?

  (has-source? (port->neighbor g '(numbo-ws target))))

(module+ test
  (let ([g (make-numbo-ws (make-graph) '(4 5 6) 15)])
    (check-false (done? g))
    (let ([g (do-graph-edits g '((:begin (:node number 9) (:node operator + +)
                                   (:edge (4 result) (+ operands))
                                   (:edge (5 result) (+ operands))
                                   (:edge (+ result) (9 source)))))])
      (check-false (done? g))
      (let ([g (do-graph-edits g '((:let ([+ (:node operator + +)])
                                     (:edge (9 result) (+ operands))
                                     (:edge (6 result) (+ operands))
                                     (:edge (+ result) (15 source)))))])
        (check-true (done? g)) ))))

(define (nodes-missing-a-neighbor g)
  (for/list ([node (members-of g 'numbo-ws)]
             #:when (missing-a-neighbor? g node))
    node))

(define (missing-a-neighbor? g node)
  (case (class-of g node)
    [(number)
     (define sources (port->neighbors g `(,node source)))
     (define results (port->neighbors g `(,node result)))
     (or (empty? sources) (empty? results))]
    [(operator)
     (define operands (port->neighbors g `(,node operands)))
     (define results (port->neighbors g `(,node result)))
     (or (< (length operands) 2) (empty? results))]
    [else #f]))

;; Completing an instance of an archetypal group

(define (superficial-matches g from-ctx from-node to-ctx)
  (for/list ([to-node (members-of g to-ctx)]
             #:when (equal? (value-of g from-node) (value-of g to-node)))
    to-node))

(define (build/bind-actions g from-ctx from-node to-ctx)
  `((build ,from-node) ,@(for/list ([to-node (superficial-matches g
                                               from-ctx from-node to-ctx)])
                           `(bind ,from-node ,to-node))))

(define (all-binds? completion)
  (for/and ([action completion])
    (bind? action)))

(define (all-builds? completion)
  (for/and ([action completion])
    (build? action)))

(define (multiple-binds-to-same-bindee? completion)
  (define h (for/fold ([h (hash)])
                      ([action completion])
              (match action
                [`(bind ,_ ,bindee)
                 (hash-update h bindee add1 0)]
                [else h])))
  (for/or ([kv (hash->list h)])
    (match-define `(,_ . ,n) kv)
    (> n 1)))

(define (filter-out-invalid-completions completions)
  (for/list ([completion completions]
             #:when (and (not (all-binds? completion))
                         (not (all-builds? completion))
                         (not (multiple-binds-to-same-bindee? completion))))
    completion))

;TODO filter out completions that don't build anything
(define (all-possible-archetype-completions g from-ctx to-ctx)
  (local-require (only-in racket/list cartesian-product))
  (filter-out-invalid-completions
    (apply cartesian-product
      (for/list ([from-node (members-of g from-ctx)])
        (build/bind-actions g from-ctx from-node to-ctx)))))

(define (attrs-to-copy g from-ctx from-node)
  (define attrs (get-node-attrs g from-node))
  (define c (hash-ref attrs 'class (void)))
  (define v (hash-ref attrs 'value (void)))
  `((class . ,c) (value . ,v) (built-by . ,from-ctx)))

(define (do-bdx-actions g from-ctx to-ctx bdx-actions)
  (for/fold ([g g] [ht-bdx (hash)])
            ([bdx-action bdx-actions])
    (match bdx-action
      [`(bind ,from-node ,to-node)
       (values g (hash-set ht-bdx from-node to-node))]
      [`(build ,from-node)
       (let*-values ([(g to-id) (make-node
                                  g (attrs-to-copy g from-ctx from-node))]
                     [(g) (add-edge g `((,to-id member-of) (,to-ctx members)))])
         (values g (hash-set ht-bdx from-node to-id)))]
      [_ (error "try-bdx-actions")])))

(define (max-neighbors-of g port)
  (match-define `(,node ,port-label) port)
  (case port-label
    [(operands) 2] ;model-specific HACK
    [else 1]))

(define (open-port? g port)
  (< (length (port->neighboring-ports g port))
     (max-neighbors-of g port)))

; throws 'cant-make-edge
(define (try-to-make-edge g edge)
  (match-define `(,port1 ,port2) edge)
  (match-define `((,node1 ,port-label1) (,node2 ,port-label2)) edge)
  (cond
    [(has-edge? g edge)
     g]
    [(and (open-port? g port1) (open-port? g port2))
     (add-edge g edge)]
    [else (raise 'cant-make-edge)]))

; assumes that edge is a list in a certain order
(define (edge-leads-to-node? g edge node)
  (match edge
    [`((,_ ,_) (,to-node ,port-label))
      (equal? to-node node)]
    ))

(define irrelevant-port-labels (set 'activation 'canonical-node 'alternates))

; assumes that edge is a list in a certain order
(define (edge-from-irrelevant-port? g edge)
  (match-let ([`((,_ ,port-label) (,_ ,_)) edge])
    (set-member? irrelevant-port-labels port-label)))

(define (required-edges-in-ctx g node ctx)
  (for/list ([edge (node->incident-edges g node)]
             #:when (and (not (edge-from-irrelevant-port? g edge))
                         (not (edge-leads-to-node? g edge ctx))))
    edge))
         
; throws 'cant-make-edge
(define (try-bdx-actions g0 from-ctx to-ctx bdx-actions)
  (let-values ([(g ht-bdx) (do-bdx-actions g0 from-ctx to-ctx bdx-actions)])
    (for*/fold ([g g])
               ([from-node (hash-keys ht-bdx)]
                [from-edge (required-edges-in-ctx g0 from-node from-ctx)])
      (match-define `((,ignored ,port-label1) (,from-neighbor ,port-label2))
                    from-edge)
      (define bindee1 (hash-ref ht-bdx from-node))
      (define bindee2 (hash-ref ht-bdx from-neighbor))
      (try-to-make-edge g `((,bindee1 ,port-label1) (,bindee2 ,port-label2)))
      )))

(define (cant-make-edge? x)
  (eq? 'cant-make-edge x))

(define (build? x)
  (and (list? x)
       (eq? 'build (car x))))

(define (bind? x)
  (and (list? x)
       (eq? 'bind (car x))))

(define (best-completion actions->g)
  (define best-action->g (argmin (λ (ag) (count build? (car ag)))
                                 actions->g))
  (cdr best-action->g))

(define (tag-failed g node)
  (if (failed? g node)
    g
    (do-graph-edits g `((:let ([:tag (:node failed)])
                           (:edge (:tag tagged) (,node tags)))))))

(define (hacked-finish-archetype g from-ctx to-ctx)
  (define actions->g
    (for/fold ([alist '()])
              ([bdx-actions (all-possible-archetype-completions g
                              from-ctx to-ctx)])
      (with-handlers ([cant-make-edge? (λ (x) alist)])
        (cons `(,bdx-actions . ,(try-bdx-actions g from-ctx to-ctx bdx-actions))
              alist))))
  (if (empty? actions->g)
    (tag-failed g from-ctx)
    (best-completion actions->g)))

;; Making and finding archetypes

(define (equation? g node)
  (node-is-a? g 'equation node))

(define (archetypes sl)
  (port->neighbors sl '(slipnet archetypes)))

(define (value->archetype-attrs v)
  (make-immutable-hash `((class . archetype) (value . ,v))))

(define iters 0)

#;(define (archetype-of-value sl value) ;TODO Extremely inefficient
  (let loop ([atypes (archetypes sl)])
    (set! iters (add1 iters))
    (cond
      [(null? atypes)
       (void)]
      [(value-of-equal? sl value (car atypes))
       (car atypes)]
      [else (loop (cdr atypes))])))

;HACK This isn't guaranteed to work, but it's way faster than the linear
;search in the commented-out function above.
(define (archetype-of-value sl v)
  (set! iters (add1 iters))
  (define id (default-name (value->archetype-attrs v)))
  (if (has-node? sl id)
    id
    (void)))

(define (value-has-archetype? sl v)
  (let ([atype (archetype-of-value sl v)])
    (if (void? atype) #f atype)))

(define (archetype-of-node sl node)
  (case (class-of sl node)
    [(equation) node]
    [else (archetype-of-value sl (value-of sl node))]))

;If archetype already exists, returns it instead of creating a new one.
(define (make-archetype-for-value sl v)
  (let ([atype (value-has-archetype? sl v)])
    (if atype
      (values sl atype)
      (let*-values ([(sl atype) (make-node sl (value->archetype-attrs v))]
                    [(sl) (add-edge sl `((slipnet archetypes)
                                         (,atype slipnet)))])
        (values sl atype)))))

;If archetype already exists, returns it instead of creating a new one.
(define (make-archetype-for-node sl node)
  (case (class-of sl node)
    [(equation)
     (values sl node)]
    [(number letter operator fills-port fills-port-greater-result
      doubled-operands)
     (make-archetype-for-value sl (value-of sl node))]
    [else (raise-arguments-error 'make-archetype-for-node
            "node's class has no defined way to make an archetype"
            "node" node)]))

;; Making a slipnet

(define (add-activation-edge sl from-node to-node [weight 1.0])
  (add-edge sl `((,from-node activation) (,to-node activation)) weight))

(define (has-activation-edge? sl node1 node2)
  (has-edge? sl `((,node1 activation) (,node2 activation))))

(define (add-activation-edges sl from-node to-nodes [weight 1.0])
  (define from-archetype (archetype-of-node sl from-node))
  (for/fold ([sl sl])
            ([to-node to-nodes])
    (define to-archetype (archetype-of-node sl to-node))
    (add-activation-edge sl from-archetype to-archetype weight)
    #;(add-edge sl `((,from-archetype activation) (,to-archetype activation)))))

(define (add-activation-edges-for sl new-node)
  (cond
    [(tag? sl new-node)
     (add-activation-edges sl new-node (taggees-of sl new-node) 0.2)]
    [(equation? sl new-node)
     (add-activation-edges sl new-node (members-of sl new-node) 0.1)]
    [else sl]))

(define (add-archetypes-for-new-nodes slipnet g new-nodes)
  (for/fold ([sl slipnet])
            ([new-node new-nodes])
    (let-values ([(sl atype) (make-archetype-for-node sl new-node)])
      sl)))

(define (add-activation-edges-for-new-nodes sl new-nodes)
  (for/fold ([sl sl])
            ([new-node new-nodes])
    (add-activation-edges-for sl new-node)))

(define (make-slipnet . graphs)
  (define-values (sl new-nodes)
    (for/fold ([sl (make-graph '(:node (:attrs ((class . slipnet)))))]
               [new-nodes (set)])
              ([g graphs])
      (let*-values ([(sl node-map) (copy-graph-into-graph sl g)]
                    [(news) (apply set (hash-values node-map))]
                    [(sl) (add-archetypes-for-new-nodes sl g news)]
                    [(new-nodes) (set-union new-nodes news)])
        (values sl new-nodes))))
  (add-activation-edges-for-new-nodes sl new-nodes))

(module+ test
  (test-case "make-slipnet"
    (define slipnet (make-slipnet
      (make-equation-graph 4 '+ 5 9)
      (make-equation-graph 4 '+ 2 6)
      (make-equation-graph 6 '+ 9 15)))
    (check-not-false (exactly-one? (curry value-of-equal? slipnet 4)
                                   (archetypes slipnet)))
    ;There should also be archetypes for the various tags provided by
    ;make-equation-graph.
    (define atype4 (archetype-of-value slipnet 4))
    (check-false (void? atype4))
;    (pr-graph slipnet) ;DEBUG
;    (newline)
;    (println (port->neighbors slipnet '(slipnet archetypes)))
;    (newline)
;    (println (all-nodes slipnet))
    ))

;; Running the slipnet

(define (activation-edges-starting-from g nodes)
  (for*/set ([node nodes]
             #;[node (alternates-of g node)]
             [edge (port->incident-edges g `(,node activation))])
    edge))

(define (add-activation activations node amount)
  (hash-update activations
               node
               (λ (old) (min 2.0 (+ old amount)))
               0.0))

(define (get-activation activations node)
  (dict-ref activations node 0.0))

(define (sliplink-weight g from-node to-node)
  (graph-edge-weight g `((,from-node activation) (,to-node activation))))

(define (spread-activation-across-edge g initial-activations activations edge)
  (define weight (match-let ([`((,from ,_) (,to ,_)) (set->list edge)])
                   (sliplink-weight g from to)))
  (define (spread-1way activations hop)
    (match-define `((,from-node ,_) (,to-node ,_)) hop) ;TODO catch self-link?
    (define r (* 0.2 (- (random) 0.5) weight))
    (add-activation activations to-node
      (* slipnet-spreading-rate
         (+ weight r)
         (get-activation initial-activations from-node))))
  (let* ([hop (set->list edge)]
         [activations (spread-1way activations hop)]
         [activations (spread-1way activations (reverse hop))])
    activations))

(define (decay-activations activations)
  (make-immutable-hash (for/list ([kv (hash->list activations)])
                         (match-define `(,node . ,a) kv)
                         `(,node . ,(* slipnet-decay a)))))

(define (do-slipnet-timestep g initial-activations)
  (for/fold ([activations (decay-activations initial-activations)])
            ([edge (activation-edges-starting-from g
                     (dict-keys initial-activations))])
    (spread-activation-across-edge g initial-activations activations edge)))

(define (run-slipnet g initial-activations)
  (for/fold ([activations initial-activations])
            ([timestep slipnet-timesteps])
    (let ([as (maybe-suspend 'slipnet-activations
                             (do-slipnet-timestep g activations))])
      ;#R (sorted-by-cdr as)
      as)))

(module+ test
  (test-case "spreading activation"
    (define slipnet (make-slipnet
      (make-equation-graph 4 '+ 5 9)
      (make-equation-graph 4 '+ 2 6)
      (make-equation-graph 6 '+ 9 15)))
    (define initial-activations #hash(
      (archetype4 . 1.0)
      (archetype5 . 1.0)
      (archetype-fills-port-4-result . 1.0)
      (archetype-fills-port-5-result . 1.0)))
    ;(pr-graph slipnet) ;DEBUG
    (define activations (run-slipnet slipnet initial-activations))
    ;#R (sorted-by-cdr activations)
    (check-equal?
      (sequence->list
        (filter (λ (node) (group? slipnet node))
                (map car (sort (hash->list activations)
                               (λ (a1 a2) (> (cdr a1) (cdr a2)))))))
      '(4+5=9 4+2=6 6+9=15)
      "group with 4 and 5 in it didn't get strongest activation")))

;; Desiderata and diagnosis

(define (problem-tag? g tag)
  (or (eq? 'need (safe-car (value-of g tag)))
      (eq? 'doubled-operands (safe-car (value-of g tag))))) ;HACK


(define (problem-tags g node)
  (for/list ([tag (tags-of g node)]
             #:when (problem-tag? g tag))
    tag))

(define (problem-tag->solution-tag g tag node)
  (match (value-of g tag)
    ['(need result)
     `(fills-port ,(value-of g node) result)]
    ['(need source)
     `(fills-port ,(value-of g node) source)]
    ['(need greater-result)
     `(fills-port-greater-result ,(value-of g node))]
    [`(doubled-operands ,_) ;HACK: This is not a problem
     (value-of g tag)]      ;      and this is not a solution
    [_ (error (~a `(problem-tag->solution-tag ,tag ,node)))]))

(define (whats-your-problem g node)
  (for*/set ([tag (problem-tags g node)]
             [archetype (list (archetype-of-value g
                                (problem-tag->solution-tag g tag node)))]
                                ;superfluous list in lieu of #:define
            #:when (not (void? archetype)))
    archetype))

;; Tagging

(define (number-node? g node)
  (node-is-a? g 'number node))

(define (needs-source? g node)
  (define need? (and (number-node? g node)
                     (empty? (port->neighbors g `(,node source)))))
  `((need source) ,need?))

(define (needs-result? g node)
  (define need? (and (number-node? g node)
                     (empty? (port->neighbors g `(,node result)))))
  `((need result) ,need?))

(define (>-mates g node)
  (for*/list ([tag (tags-of g node)]
              #:when (and (node-is-a? g '> tag)
                          (equal? node (port->neighbor g `(,tag tagged-b)))))
    (port->neighbor g `(,tag tagged-a))))

(define (needs-greater-result? g node)
  (and (has-tag? g '(need result) node)
       (for/or ([mate (>-mates g node)])
         (has-tag? g '(need source) mate))))

(define (tags-for-relations g node1 node2)
  (case (list (class-of g node1) (class-of g node2))
    [((number number))
     (define n1 (value-of g node1))
     (define n2 (value-of g node2))
     (cond
       [(> n1 n2) '(>)]
       [(= n1 n2) '(=)]
       [else '()])]
    [else (error 'tags-for-relations)]))

(define (tags-for-relational-need g node)
  (cond
    [(needs-greater-result? g node)
     '((need greater-result))]
    [else '()]))

(define (number-nodes-in g ctx)
  (for/list ([node (members-of g ctx)]
             #:when (number-node? g node))
    node))

(define (accumulate-tags tag-funcs g . nodes)
  (for*/list ([tag-func tag-funcs]
              [tag (apply tag-func g nodes)])
    tag))

(define (add-simplest-tags g ctx)
  (for*/fold ([g g])
             ([node (number-nodes-in g ctx)]
              [info (list (needs-source? g node)
                          (needs-result? g node))])
    (match-define `(,tag ,need?) info)
    (define already-has? (has-tag? g tag node))
    (if need?
      (if already-has?
        g
        (do-graph-edits g `((add-tag ,tag ,node)
                            (boost-salience ,node))))
      (if (has-tag? g tag node)
        ;(remove-node g already-has?) ;HACK: should go through rewrite
        (do-graph-edits g `((:remove-node ,already-has?)
                            (reduce-salience ,node)))
                      ;HACK: effect on salience should depend on other factors
        g))))

(define (add-relation-tags g ctx)
  (for*/fold ([g g])
             ([node1 (number-nodes-in g ctx)]
              [node2 (number-nodes-in g ctx)]
              #:when (not (equal? node1 node2))
              [tag (accumulate-tags (list tags-for-relations) g node1 node2)])
    (do-graph-edits g `((add-tag2 ,tag ,node1 ,node2)))))

(define (add-relational-need-tags g ctx)
  (for*/fold ([g g])
             ([node (number-nodes-in g ctx)]
              [tag (accumulate-tags (list tags-for-relational-need) g node)])
    (do-graph-edits g `((add-tag ,tag ,node)))))

(define (nodes-that-need-result g ctx)
  (for/list ([node (number-nodes-in g ctx)]
             #:when (needs-result? g ctx))
    node))

(define (add-doubled-operand-tags g ctx)
  (for*/fold ([g g])
             ([node1 (nodes-that-need-result g ctx)]
              [node2 (nodes-that-need-result g ctx)]
              #:when (not (eq? node1 node2)))
    (if (= (value-of g node1) (value-of g node2))
      (do-graph-edits g `((add-tag (doubled-operands ,(value-of g node1))
                                   ,node1 ,node2)))
      g)))

(define (tag-all-numbers g ctx)
  (let* ([g (add-simplest-tags g ctx)]
         [g (add-relation-tags g ctx)]
         [g (add-relational-need-tags g ctx)]
         [g (add-doubled-operand-tags g ctx)])
    g))

(define (untag-all-numbers g ctx)
  (for*/fold ([g g])
             ([node (number-nodes-in g ctx)]
              [tag (tags-of g node)])
   (do-graph-edits g `((:remove-node ,tag)))))

;; Running

(define (archetypes-to-activate-for g node)
  (define w (whats-your-problem g node))
  (if (empty? w)
    '()
    (cons (archetype-of-node g node) (set->list w))))

(define (archetype-salience-factor g archetype) ;HACK
  (match (value-of g archetype)
    [`(doubled-operands . ,_) 0.1]
    [`(fills-port ,_ result) 2.0]
    [`(fills-port ,_ source) 2.0]
    [`(fills-port-greater-result ,_) 2.0]
    [else 0.1]))

(define (make-initial-activations g)
  (for/fold ([h (hash)]) ; (archetype . activation)
            ([node (members-of g 'numbo-ws)])
    (define s (salience-of g node))
    (if (zero? s)
      h
      (for/fold ([h h])
                ([archetype (archetypes-to-activate-for g node)])
        (hash-update h
                     archetype
                     (λ (old)
                       (+ old (* (archetype-salience-factor g archetype)
                                 s)))
                     0.0)))))

(define (failed? g node)
  (for/or ([neighbor (port->neighbors g `(,node tags))]
           #:when (eq? 'failed (class-of g neighbor)))
    neighbor))

(define (candidate-group? g slipnode)
  (and (group? g slipnode)
       (not (failed? g slipnode))))

(define (most-active-group g activations)
  (define group-activations (for/list ([g-a (hash->list activations)]
                                       #:when (candidate-group? g (car g-a)))
                              g-a))
  (when (empty? group-activations)
    (raise 'nothing-to-do))
  (car (argmax cdr group-activations)))

;TODO Move this to a util file DONE rm this
(define (sorted xs)
  (sort (hash->list xs) string<? #:key ~a))

(define (sorted-by-cdr ht)
  (sort (hash->list ht) < #:key cdr))

(define (search-slipnet g initial-activations)
  (define activations (run-slipnet g initial-activations))
  ;#R (sorted-by-cdr activations)
  (most-active-group g activations))

(define salience-decay 0.8)

(define (salience-of g node)
  (let ([s (get-node-attr g node 'salience)])
    (if (void? s) 0.0 s)))

(define (update-saliences g)
  (for/fold ([g g])
            ([node (members-of g 'numbo-ws)])
;    (define new-salience (+ (* salience-decay (salience-of g node))
;                            (if (missing-a-neighbor? g node) 1.0 0.0)))
    (define new-salience (* salience-decay (salience-of g node)))
    (set-node-attr g node 'salience new-salience)))

(define (print-saliences g)
  (for ([node (members-of g 'numbo-ws)])
    (displayln @~a{@node @(salience-of g node)})))

(define (saliences-ht g)
  (for/hash ([node (members-of g 'numbo-ws)])
    (values node (salience-of g node))))

(define (eq?f x)
  (λ (x*) (eq? x x*)))

(define (log . args)
  (define as (string-join (for/list ([arg args])
                            (~a arg))))
  (displayln @~a{  @as}))

(define (decayable-node? g node)
  (or (eq? 'failed (class-of g node))
      (and (member-of? g 'numbo-ws node)
           (not (brick? g node))
           (not (target? g node)))))

(define (decayable-nodes g)
  (for/list ([node (all-nodes g)]
             #:when (decayable-node? g node))
    node))

(define (decay-support g)
  (for/fold ([g g])
            ([node (decayable-nodes g)])
    (define support (get-node-attr g node 'support))
    (if (void? support)
      (set-node-attr g node 'support 1.0)
      (let ([new-support (* support-decay-rate support)])
        (if (< new-support 0.2)
          (begin
            ;(log (format "removing ~a" node))
            (remove-node g node))
          (set-node-attr g node 'support new-support))))))

(define (built? g node)
  (node-attr? g 'built-by node))

(define (block? g node)
  (and (number-node? g node)
       (built? g node)))

(define (blocks-in g ctx)
  (for/list ([node (members-of g ctx)]
             #:when (block? g node))
    node))

(define (bricks-in g ctx)
  (port->neighbors g `(,ctx bricks)))

(define (number-of-unused-bricks g ctx)
  (for/sum ([brick (bricks-in g ctx)])
    (if (second (needs-result? g brick)) 1 0)))

(define (number-of-holes g ctx)
  (for/sum ([block (blocks-in g ctx)])
    (cond
      [(second (needs-source? g block))
       2]
      [(second (needs-result? g block))
       1]
      [else 0])))

(define (number-of-sources-needed g ctx)
  (for/sum ([block (blocks-in g ctx)])
    (cond
      [(second (needs-source? g block))
       2]
      [else 0])))

(define (clear-blocks g ctx)
  (for/fold ([g g])
            ([node (members-of g ctx)]
             #:when (built? g node))
    (let* ([g (tag-failed g (get-node-attr g node 'built-by))]
           [g (do-graph-edits g `((:remove-node ,node)))])
      g)))

(define (clear-blocks-if-hopeless g ctx)
  (if (> (number-of-sources-needed g ctx) (number-of-unused-bricks g ctx))
    (clear-blocks g ctx)
    g))

(define (do-timestep g)
  (with-handlers ([(eq?f 'nothing-to-do) (λ (_) (log "Nothing to do.") g)])
    (let* (;[_ (pr-group g 'numbo-ws)]
           [g (clear-blocks-if-hopeless g 'numbo-ws)]
           [g (decay-support g)]
           [g (untag-all-numbers g 'numbo-ws)] ;HACKKKKKKK
           [g (tag-all-numbers g 'numbo-ws)]
           ;[_ (pr-group g 'numbo-ws)] ;DEBUG
           [g (update-saliences g)]
           [_ (maybe-suspend 'g g)]
           [_ (maybe-suspend 'numbo-ws (saliences-ht g))]
           ;[_ (print-saliences g)] ;DEBUG
           [activations (maybe-suspend 'slipnet-activations
                                       (make-initial-activations g))]
           [archetypal-group (search-slipnet g activations)]
           [_ (log "trying" archetypal-group)]
           [g (hacked-finish-archetype g archetypal-group 'numbo-ws)])
    g)))

(define (initialize-salience g)
  (for/fold ([g g])
            ([node (members-of g 'numbo-ws)])
    (set-node-attr g node 'salience 0.0)))

(define (result-of g node)
  (port->neighbor g `(,node result)))

(define (source-of g node)
  (port->neighbor g `(,node source)))

(define (operands-of g node)
  (port->neighbors g `(,node operands)))

(define (brick? g node)
  (eq? 'numbo-ws (source-of g node)))

(define (target? g node)
  (eq? 'numbo-ws (result-of g node)))

(define (result-expr g)
  (define target (port->neighbor g '(numbo-ws target)))
  (list '= target
    (let loop ([node target])
      (case (class-of g node)
        [(number)
         (if (brick? g node)
           node
           (loop (source-of g node)))]
        [(operator)
         (list* (value-of g node)
                (for/list ([operand (operands-of g node)])
                  (loop operand)))]))))

(random-seed 0)

(define (run^ g)
  (maybe-suspend 'g g)
  (maybe-suspend 'numbo-ws (saliences-ht g))
  (with-handlers ([(λ (e) (match e
                            [`(done ,_) #t]
                            [else #f]))
                   (λ (e) (cadr e))])
    (for/fold ([g g])
              ([t max-timesteps])
      (if (done? g)
        (begin
          (log (result-expr g))
          (raise `(done ,g)))
        (maybe-suspend 'g (do-timestep g))))))

(define (make-start-graph bricks target slipnet)
  (let*-values ([(g) (make-graph)]
                [(g) (make-numbo-ws g bricks target)]
                [(g _) (copy-graph-into-graph g slipnet)])
    g))

(define (run bricks target [slipnet medium-slipnet])
  (run^ (make-start-graph bricks target slipnet)))

;; Memorized arithmetic

(define (make-equation-graph n1 op n2 result)
  (let ([group-name (string->symbol (format "~a~a~a=~a" n1 op n2 result))]
        [n2-name (if (equal? n1 n2) (string->symbol (format "~aa" n2)) n2)]
        [result-name (if (or (equal? result n1) (equal? result n2))
                       (string->symbol (format "~ar" result))
                       result)])
    (make-graph `(:let ([:equation
                          (:group (:name ,group-name) (:class equation)
                            (:node (:attrs ((class . number)
                                            (value . ,n1)
                                            (name .  ,n1))))
                            (:node (:attrs ((class . number)
                                            (value . ,n2)
                                            (name .  ,n2-name))))
                            (:node (:attrs ((class . operator)
                                            (value . ,op)
                                            (name . ,op))))
                            (:node (:attrs ((class . number)
                                            (value . ,result)
                                            (name .  ,result-name))))
                            (:edge (,n1 result) (,op operands))
                            (:edge (,n2-name result) (,op operands))
                            (:edge (,op result) (,result-name source)))])
                    (add-tag (fills-port ,n1 result) :equation)
                    ,@(if (> result n1)
                       `((add-tag (fills-port-greater-result ,n1) :equation))
                       '())
                    (add-tag (fills-port ,n2 result) :equation)
                    ,@(if (> result n2)
                       `((add-tag (fills-port-greater-result ,n2) :equation))
                       '())
                    (add-tag (fills-port ,op result) :equation)
                    (add-tag (fills-port ,op operands) :equation)
                    (add-tag (fills-port ,result source) :equation)
                    ,@(if (equal? n1 n2)
                        `((add-tag (doubled-operands ,n1) :equation))
                        '())
                    ))))

#;(define slipnet (make-slipnet
  (make-graph '(:group 4+5=9 4 5 + 9
                 (:edge (4 result) (+ operands))
                 (:edge (5 result) (+ operands))
                 (:edge (+ result) (9 source))))
  (make-graph '(:group 4+2=6 4 2 + 6
                 (:edge (4 result) (+ operands))
                 (:edge (2 result) (+ operands))
                 (:edge (+ result) (6 source))))
  (make-graph '(:group 1+1=2 1 1 + 2
                 (:edge (1 result) (+ operands))
                 (:edge (1a result) (+ operands))
                 (:edge (+ result) (2 source))))
  (make-graph '(:group 6+9=15 6 9 + 15
                 (:edge (6 result) (+ operands))
                 (:edge (9 result) (+ operands))
                 (:edge (+ result) (15 source))))))

(define slipnet (make-slipnet
  (make-equation-graph 4 '+ 5 9)
  (make-equation-graph 4 '+ 2 6)
  (make-equation-graph 1 '+ 1 2)
  (make-equation-graph 6 '+ 9 15)))

(define (commutative? op)
  (case op
    [(+ *) #t]
    [(- /) #f]))

;; Returns a list of graphs, one for each equation.
(define (make-memorized-arithmetic-tables n [no-greater-than 361])
  (define ns (make-base-namespace))
  (define operand-pairs (for*/set ([i (in-range 1 (add1 n))]
                                   [j (in-range 1 (add1 n))])
                          `(,i ,j)))
  (define (make-result ij op)
    (match-define `(,i ,j) ij)
    (define expr `(,op ,i ,j))
    (eval expr ns))
  (define tuples (for*/fold ([tuples '()])
                            ([ij operand-pairs]
                             [op '(+ - *)]
                             [result (list (make-result ij op))]
                             #:when (and (<= result no-greater-than)
                                         (not (and (eq? '* op)
                                                   (or (= 1 (car ij))
                                                       (= 1 (cadr ij)))))))
                   (match-define `(,i ,j) ij)
                   (cond
                     [(negative? result)
                       tuples]
                     [(and (commutative? op) (< i j))
                       tuples]
                     [else (cons `(,i ,op ,j ,result) tuples)])))
  (for/list ([tuple tuples])
    (apply make-equation-graph tuple)))

(define (commutative-redundant? tuple)
  (match-define `(,op ,i ,j) tuple)
  (and (commutative? op) (< i j)))

(define (remove-tuple-dups tuples)
  (for/list ([tuple (list->set tuples)]
             #:when (not (commutative-redundant? tuple)))
    tuple))

(define (tuples->equations tuples)
  (define ns (make-base-namespace))
  (for/list ([tuple (remove-tuple-dups tuples)])
    (match-define `(,op ,i ,j) tuple)
    (define result (eval tuple ns))
    (apply make-equation-graph `(,i ,op ,j ,result))))

(set! iters 0)

(define (numeric-archetypes sl)
  (for/list ([archetype (archetypes sl)]
             #:when (number? (value-of sl archetype)))
    archetype))

(define (log10 x)
  (logarithm x 10.0))

(define (numeric-closeness n m)
  (min 1.0 (max 0.0
    (- 1.0 (* 3.0 (abs (- (log10 n) (log10 m))))))))

(define (add-edges-between-number-archetypes sl)
  (for*/fold ([sl sl])
             ([n-id (in-list (numeric-archetypes sl))]
              [m-id (in-list (numeric-archetypes sl))]
              #:when (and (not (zero? (value-of sl n-id)))
                          (not (zero? (value-of sl m-id))))
              #:when (not (has-edge? sl `((,n-id activation)
                                          (,m-id activation))))
              #:when (not (equal? n-id m-id)))
      (let-values ([(n m) (apply values (sort (list (value-of sl n-id)
                                                    (value-of sl m-id)) <))])
        (define nc (* 2.0 (numeric-closeness n m)))
        (define ns (~a n))
        (define ms (~a m))
        (define length-closeness (if (= (string-length ns)
                                       (string-length ms))
                                    0.2
                                    0.0))
        (define 1st-digit-closeness
          (let ([n1 (substring ns 0 1)]
                [m1 (substring ms 0 1)])
            (if (equal? n1 m1) 0.6 0.0)))
        (define last-digit-closeness
          (let ([nlast (substring ns (sub1 (string-length ns))
                                     (string-length ns))]
                [mlast (substring ms (sub1 (string-length ms))
                                     (string-length ms))])
            (if (equal? nlast mlast) 0.3 0.0)))
        (define weight (+ nc length-closeness 1st-digit-closeness
                          last-digit-closeness))
        (add-edge sl `((,n-id activation) (,m-id activation)) weight))))

(define (add-edges-between-close-numbers sl)
  (for*/fold ([sl sl])
             ([n-id (in-list (numeric-archetypes sl))]
              [m-id (in-list (numeric-archetypes sl))]
              [n (list (value-of sl n-id))]
              [m (list (value-of sl m-id))]
              #:when (and (not (= n m))
                          (<= (abs (- n m)) 3)
                          (not (has-activation-edge? sl n-id m-id))))
    (add-activation-edge sl n-id m-id (case (abs (- n m))
                                        [(1) 1.0]
                                        [(2) 0.9]
                                        [(3) 0.8]))))

(define (first-number seq)
  (for/or ([x seq])
    (if (number? x) x #f)))

(define (archetypes-that-refer-to-numbers sl)
  (for/list ([archetype (archetypes sl)]
             #:when (let ([v (value-of sl archetype)])
                      (and (list? v) (ormap number? v))))
    archetype))

;; Archetypes like (fills-port X ...)
(define (archetypes-that-refer sl)
  (for/list ([archetype (archetypes sl)]
             #:when (let ([v (value-of sl archetype)])
                      (and (list? v) (>= (length v) 2))))
    archetype))

(define (add-edges-to-number-archetypes-from-referring-archetypes sl)
  (for*/fold ([sl sl])
             ([archetype (archetypes-that-refer-to-numbers sl)])
    (define n (first-number (value-of sl archetype)))
    (define n-id (archetype-of-value sl n))
    (if (void? n-id) sl (add-activation-edge sl archetype n-id 0.1))))

;; Adds activation edges from (fills-port X ...) to X, etc.
(define (add-edges-from-referring-archetypes sl)
  (for*/fold ([sl sl])
             ([archetype (archetypes-that-refer sl)])
    (define referent (second (value-of sl archetype)))
    (define referent-archetype (archetype-of-value sl referent))
    (if (void? referent-archetype)
      sl
      (add-activation-edge sl archetype referent-archetype 0.1))))


(define (add-edges-for-tag-with-arg sl tagname)
  (let-values ([(sl atype) (make-archetype-for-value sl tagname)])
    (for*/fold ([sl sl])
               ([a (archetypes sl)]
                #:when (let ([v (value-of sl a)])
                         (and (list? v) (equal? tagname (car v)))))
      (add-activation-edge sl atype a 1.0))))

(define (add-activation-edges-for-arithmetic sl)
  (let* ([sl (add-edges-from-referring-archetypes sl)]
         [sl (add-edges-between-close-numbers sl)]
         [sl (add-edges-for-tag-with-arg sl 'fills-port-greater-result)]
         [sl (add-activation-edge sl 'archetypefills-port-greater-result
                                     'archetype* 6.0)])
    sl))

(define (make-slipnet-for-arithmetic n [no-greater-than 361])
  (let* ([sl (apply make-slipnet
                    (make-memorized-arithmetic-tables n no-greater-than))]
         ;[sl (add-edges-between-number-archetypes sl)]
         ;[sl (add-edges-between-close-numbers sl)]
         ;[sl (add-edges-to-number-archetypes-from-referring-archetypes sl)]
;         [sl (add-edges-from-referring-archetypes sl)]
;         [sl (add-edges-for-tag-with-arg sl 'fills-port-greater-result)]
;         [sl (add-activation-edge sl 'archetypefills-port-greater-result
;                                     'archetype* 6.0)]
         )
    (add-activation-edges-for-arithmetic sl)))

(define (.. lb ub [step 1])
  (range lb (add1 ub) step))

(define elementary-equation-tuples
  (append
    (cartesian-product
      '(+ - *)
      '(0 1 2 3 4 5 6 7 8 9 10)
      '(0 1 2 3 4 5 6 7 8 9 10))
    (cartesian-product
      '(+ - *)
      '(0 1 2 3 4 5 6 7 8 9 10)
      '(10))
    (cartesian-product
      '(+ -)
      (.. 0 100)
      '(0 1 2 3))
    (cartesian-product
      '(*)
      (.. 10 100 10)
      '(0 1 2 3 4 5 6 7 8 9 10))
    ))

;(define medium-slipnet (make-slipnet-for-arithmetic 12))
(define medium-slipnet
  (add-activation-edges-for-arithmetic
    (apply make-slipnet (tuples->equations elementary-equation-tuples))))

(define (make-big-slipnet)
  (make-slipnet-for-arithmetic 25 361))

#;(begin
  #R iters
  #R (length (archetypes big-slipnet))
  (void))

;(define g (run '(4 5 6) 15 big-slipnet))
;(pr-group g 'numbo-ws)
;(define h (run '(1 1) 2))

;(define g (make-start-graph '(4 5 6) 15 slipnet))
;(define g1 (do-timestep g))