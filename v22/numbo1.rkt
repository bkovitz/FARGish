; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         "model1.rkt"
         "shorthand.rkt"
         "slipnet1.rkt"
         "completion1.rkt"
         "crawl.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require racket/set racket/hash)
(require rackunit racket/pretty describe profile racket/enter racket/trace)

(provide (all-defined-out))

;; Global constants

(random-seed 0)

;(define max-timesteps 10)
;(define support-decay-rate 0.5)

;; ======================================================================
;;
;; The spec
;;

(define spec
  (farg-model-spec
    (nodeclass numbo-ws
      (is-a 'ctx))
    (nodeclass (equation nm)
      (is-a 'ctx)
      (name nm)
      (archetype is-node))
    (nodeclass (number n)
      (name n)
      (value n)
      (archetype n))
    ;(make-nodeclass (lambda (n) (list (name n) (value n) (archetype n)))
    (nodeclass (target n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'target 'result) as-member))
    (nodeclass (brick n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (nodeclass operator
      (archetype is-class))
    (nodeclass +
      (is-a 'operator))
    (nodeclass -
      (is-a 'operator))
    (nodeclass *
      (is-a 'operator))
    (nodeclass /
      (is-a 'operator))
    (tagclass problem-tag)
    (tagclass (equation-tag x)
      (applies-to ([node (of-class 'equation) (by-ports 'tagged 'tags)])
        (condition (const #t)))) ;HACK condition should be optional
    (tagclass (result x)  ;NEXT It looks like inheriting exposed the args bug:
                          ;'equation doesn't take an x. What do we do about
                          ;this?
      (is-a 'equation-tag)
      (archetype `(result ,x)))
    (tagclass (has-operand x)
      (is-a 'equation-tag)
      (archetype `(has-operand ,x)))
    (tagclass (result-digits n)
      (is-a 'equation-tag)
      (archetype `(result-digits ,n)))
    (tagclass (needs need-type)
      (is-a 'problem-tag)
      (name (string->symbol @~a{needs-@need-type}))
      (archetype `(needs ,need-type))
      (value `(needs ,need-type))
      (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
        (condition (no-neighbor-at-port?/g need-type node))))
    (tagclass (fills-port result n)
      (is-a 'solution-tag)
      (archetype `(fills-port ,result ,n))
      (applies-to ([node (of-class 'equation) (by-ports 'tagged 'tags)])
        (condition (const #t))) ;HACK condition should be optional
      )
    (tagclass (num-digits n)
      (applies-to ([node (of-class 'number)])
        (condition (const #t))))
        ; TODO Should be something like this:
        ; (condition (=/g n (num-digits-of-value/g node)))
    (tagclass (decade n)
      (applies-to ([node (of-class 'number)])
        (condition (const #t))))
    (tagclass (tried atype)
      (value atype)
      (applies-to ([node (by-ports 'tagged 'tags)])
        (condition (const #t))))
    (portclass source
      (max-neighbors 1))
    (portclass result
      (max-neighbors 1))
    ))
;      (applies-to ([node (by-ports 'tagged 'tags)])
;        (condition (has-neighbor-at-port?/g result node)))

;; ======================================================================
;;
;; Numbo graph and workspace
;;

(define empty-numbo-graph
  (make-graph spec '(ws) '(slipnet)))

(define (start-numbo-ws g bricks target)
  (let*-values ([(g) (remove-nodes/in g 'ws)]
                [(g) (add-nodes/in g 'ws 'brick bricks)]
                [(g) (add-node/in g 'ws 'target target)])
    g))

;(module+ test
;  (test-case "numbo-ws"
;    (let*-values ([(g) (make-empty-graph spec)]
;                  ;[(_) (pretty-print (get-spec g))]
;                  [(g ws) (make-numbo-ws g '(4 5 6) 15)])
;      ;(pr-graph g)
;      ;TODO
;      (void)
;      )))

;; ======================================================================
;;
;; Searching the numbo-ws
;;

;(define (done? g)
;  (define (has-source? node)
;    (cond
;      [(node-is-a? g node 'number)
;       (define sources (g:port->neighbors g `(,node source)))
;       (if (empty? sources)
;         #f
;         (for/and ([source sources])
;           (has-source? source)))]
;      [(node-is-a? g node 'operator)
;       (define operands (g:port->neighbors g `(,node operands)))
;       (if (empty? operands)
;         #f
;         (for/and ([operand operands])
;           (has-source? operand)))]
;      [(node-is-a? g node 'ws)
;       #t]))
;  ;TODO? throw error if no match?
;
;  (has-source? (g:port->neighbor g '(ws target))))

(define (numbers-in g ctx)
  (for/list ([node (members-of g ctx)]
             #:when (node-is-a? g node 'number))
    node))

(define (done? g)
  (define target (g:port->neighbor g '(ws target)))
  (define target-number (value-of g target))
  (define (has-source? node)
    (cond
      [(node-is-a? g node 'number)
       (define sources (g:port->neighbors g `(,node source)))
       (if (empty? sources)
         #f
         (for/and ([source sources])
           (has-source? source)))]
      [(node-is-a? g node 'operator)
       (define operands (g:port->neighbors g `(,node operands)))
       (if (empty? operands)
         #f
         (for/and ([operand operands])
           (has-source? operand)))]
      [(node-is-a? g node 'ws)
       #t]))
  ;TODO? throw error if no match?

  (for/or ([node (numbers-in g 'ws)])
    (and (= target-number (value-of g node))
         (has-source? node))))


;(module+ test
;  (let ([g (make-numbo-g '(4 5 6) 15)])
;    (check-false (done? g))
;    
;
;
;    (let ([g (g:do-graph-edits g '((:begin (:node number 9) (:node operator + +)
;                                   (:edge (4 result) (+ operands))
;                                   (:edge (5 result) (+ operands))
;                                   (:edge (+ result) (9 source)))))])
;      (check-false (done? g))
;      (let ([g (g:do-graph-edits g '((:let ([+ (:node operator + +)])
;                                     (:edge (9 result) (+ operands))
;                                     (:edge (6 result) (+ operands))
;                                     (:edge (+ result) (15 source)))))])
;        (check-true (done? g)) ))))

;; ======================================================================
;;
;; Equation graph
;;

; Adds an 'equation group containing the result, sourced to expr. Makes a
; new node for each number and operator. Adds edges between appropriate
; 'source and 'result ports.
(define (make-equation g result- expr)
  (let*-values ([(g equation) (make-node g
                                'equation (equation-name result- expr))]
                [(g result) (make-node/in g equation 'number result-)]
                [(g operator) (make-expr/in g equation expr)]
                [(g) (g:add-edge g `((,operator result) (,result source)))])
    (values g equation)))
                
(define (make-expr/in g equation expr)
  (match-define `(,operator- . ,operands) expr)
  (let-values ([(g operator) (make-node/in g equation operator-)])
    (for/fold ([g g] #:result (values g operator))
               ([operand operands])
      (let*-values ([(g operand) (make-node/in g equation 'number operand)]
                    [(g) (g:add-edge g
                           `((,operand result) (,operator operands)))])
        g))))

(define (equation-name result expr)
  (string->symbol
    (string-append
      (string-join (map ~a (cdr expr)) (~a (car expr)))
      @~a{=@result})))


(define (.. lb ub [step 1])
  (range lb (add1 ub) step))

(define elementary-equation-exprs
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

(define base-namespace (make-base-namespace))
(define (eval-expr expr)
  (eval expr base-namespace))

(define (remove-redundant-exprs exprs)
  (for/fold ([ht empty-hash] #:result (hash-values ht))
            ([expr exprs])
    (match-define `(,op . ,operands) expr)
    (define key
      (case op
        [(+ *) `(,op ,(apply set operands))]
        [(- /) expr]))
    (if (hash-has-key? ht key)
      ht
      (hash-set ht key expr))))

;TODO UT
(define (add-memorized-equations g exprs)
  (for/fold ([g g])
            ([expr (remove-redundant-exprs exprs)])
    (add-memorized-equation g (list (eval-expr expr) expr))))

(define (add-memorized-equation g eqn)
  (let*-values ([(g equation) (apply make-equation g eqn)]
                [(g) (add-equation-tags g equation)])
    (add-group-to-slipnet g equation)))

(module+ test
  (test-case "make-equation"
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))

    (define (value-of/g node) (value-of g node))
    (define (class-of/g node) (class-of g node))
    (define (source-of g node) (g:port->neighbor g `(,node source)))
    (define (result-of g node) (g:port->neighbor g `(,node result)))

    (check-equal? (sorted (map value-of/g (members-of g equation)))
                  (list (void) 2 2 4))
    (check-equal? (sorted (map class-of/g (members-of g equation)))
                  '(+ number number number))))

(define (args->number g node)
  (match (args-of g node)
    [(list n) #:when (integer? n)
     n]
    [else #f]))

(define (first-digit n)
  (let loop ([n (floor (abs n))])
    (cond
      [(<= n 9) n]
      [else (loop (truncate (/ n 10)))])))

;HACK Archetypes should really have attrs that tell what they're archetypes of.
(define (number-archetype? g node)
  (and (eq? 'archetype (class-of g node))
       (args->number g node)))

(define (number-archetypes g)
  (for/list ([a (archetypes g)]
             #:when (number-archetype? g a))
    a))

(define (add-tags-to-number-archetypes g)
  (for/fold ([g g])
            ([archetype (number-archetypes g)])
    (define n (args->number g archetype))
    (define num-digits (string-length (~a (abs n))))
    (define decade (cond
                     [(negative? n) #f]
                     [(= 1 num-digits) 0]
                     [(= 2 num-digits) (first-digit n)]
                     [else #f]))
    (let* ([g (link-archetypally g archetype `(num-digits ,num-digits) 2.0)]
           [g (if decade
                (link-archetypally g archetype `(decade ,decade) 2.0)
                g)])
      g)))

;(define (add-fills-port-tags g equation)
;  (for*/fold ([g g])
;             ([node (members-of g equation)]
;              [port-label `(source result)])
;    (let ([value (value-of g node)])
;      (cond
;        [(void? value) g]
;        [else
;          (if (g:port-has-neighbor? g `(,node ,port-label))
;            (add-tag g `(fills-port ,value ,port-label) equation)
;            g)]))))
    
;(define (add-equation-tags g equation)
;  (let* ([g (add-fills-port-tags g equation)])
;    g))

(define ht-port-label->tag
  #hash((result . result)
        (operands . has-operand)))

(define (add-equation-tags g equation)
  (let ([operator (find-in g 'operator equation)])
    (if (void? operator)
      g
      (for*/fold ([g g])
                 ([port-label '(result operands)]
                  [neighbor (g:port->neighbors g `(,operator ,port-label))])
        (define tagclass (hash-ref ht-port-label->tag port-label))
        (define tagspec `(,tagclass ,(value-of g neighbor)))
        (add-tag-even-if-already-there g tagspec equation)
        ))))

(module+ test
  (test-case "add-equation-tags"
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))
    
    (gdo add-equation-tags equation)))

;; ======================================================================
;;
;; Searching the slipnet
;;

(define (search-slipnet g initial-activations)
  (let* ([g (g:graph-set-var g 'initial-activations initial-activations)]
         [activations (run-slipnet g initial-activations)]
         [g (g:graph-set-var g 'activations activations)])
    ;#R (take-right (sorted-by-cdr activations) 20)
    (values g (most-active-equation g activations))))

(define (most-active-equation g activations)
  (define eqn-activations (for/list ([e-a (hash->list activations)]
                                     #:when
                                       (and (g:has-node? g (car e-a))
                                            (node-is-a? g (car e-a) 'equation)))
                            e-a))
  (when (null? eqn-activations)
    (raise 'nothing-to-do))
  (car (argmax cdr eqn-activations)))

;TODO Move to model1.rkt
(define (neighborhood-around g node [max-steps 3])
  (let loop ([num-steps 1]
             [result (set `(,node 0))]
             [neighbors (g:node->neighbors g node)]
             [already-visited (set node)])
    (cond
      [(> num-steps max-steps) result]
      [(set-empty? neighbors) result]
      [else (let* ([new-neighbors (set-subtract neighbors already-visited)]
                   [result (set-union result (for/set ([neighbor new-neighbors])
                                               `(,neighbor ,num-steps)))])
              (loop (add1 num-steps)
                    result
                    (apply set-union empty-set
                                     (for/list ([neighbor new-neighbors])
                                       (g:node->neighbors g neighbor)))
                    (set-union already-visited new-neighbors)))])))

;(define (make-initial-activations g focal-node)
;  (for/fold ([ht (hash)]) ; (archetype . activation)
;            ([pair (neighborhood-around g focal-node)])
;    (match-define `(,node ,num-steps) pair)
;    (define activation (* (salience-of g node) (expt 0.95 num-steps)))
;    (if (zero? activation)
;      ht
;      (for/fold ([ht ht])
;                ([archetype (without-voids (relevant-archetypes g node))])
;        (hash-update ht archetype (λ (old) (+ (* 0.9 old) activation)) 0.0)))))

;HACK
(define (make-initial-activations g)
  (let* ([ht empty-hash]
         [target (g:port->neighbor g '(ws target))]
         [archetype (f:archetype-name `(result ,(value-of g target)))]
         [ht (hash-set ht archetype 5.0)])
    (for/fold ([ht ht])
              ([brick (g:port->neighbors g '(ws bricks))])
      (define archetype (f:archetype-name `(has-operand ,(value-of g brick))))
      (hash-update ht archetype (λ (old) (+ old 1.0)) 0.0))))

;(define (need->fills-port g tag)
;  (define value (value-of-taggee g tag))
;  (define port-label (car (args-of g tag)))
;  `(fills-port ,value ,port-label))
;
;;TODO This should be in the spec
;(define (relevant-archetypes g node)
;  (cond
;    [(node-is-a? g node 'problem-tag)
;     (list (f:archetype-name (need->fills-port g node)))]
;    [else
;     ;HACK Should get all archetypes of node, not just first one
;     (list (archetype-of-node g node))
;     #;(map f:archetype-name (get-nodeclass-attr g node 'archetype-names))]))

;; ======================================================================
;;
;; Diagnosing problems, mapping to solutions
;;

(define problem-tags '((needs source) (needs result)))

;TODO Move to model1.rkt
(define (applicable-tags g node tagspecs)
  (for/list ([tagspec tagspecs]
             #:when (tagclass-applies-to? g tagspec node))
    tagspec))

; Returns two values: g, node-with-problem(s).
; "Side-effect": adds appropriate problem tag(s) to the node with problems.
(define (look-for-problems g ctx)
  (let-values ([(node problem-tags) (find-node-with-problems g ctx)])
    (if (void? node)
      (values g (void))
      (values (add-tags g problem-tags node) node))))

; Returns two values: node, problem-tags. If no problem found, then (void)
; (void).
(define (find-node-with-problems g ctx)
  (let loop ([nodes (seq-weighted-by-salience g (members-of g ctx))])
    (cond
      [(null? nodes) (values (void) (void))]
      [else (let* ([node (car nodes)]
                   [tagspecs (applicable-tags g node problem-tags)])
              (cond
                [(null? tagspecs) (loop (cdr nodes))]
                [else (values node tagspecs)]))])))

;HACK In lieu of a correct tags-of function.
(define (neighbor-tags g node)
  (for/list ([neighbor (g:port->neighbors g node)]
             #:when (tag? g neighbor))
    neighbor))

(define (remove-obsolete-tags g ctx)
  (for*/fold ([g g])
             ([node (members-of g ctx)]
              [tag (neighbor-tags g node)])
    (if (tag-still-applies? g tag)
      g
      (remove-tag g tag))))

(define (remove-tag g tag)
  (let ([nodes (taggees-of g tag)])
    (for/fold ([g (g:remove-node g tag)])
              ([node nodes])
      (reduce-salience-of g node))))

; ----------------------------------------------------------------------

(define (choose-search-items g ctx)
  (for/fold ([search-items '()])
            ([node (numbers-in g ctx)])
    (cond
      [(g:no-neighbor-at-port? g 'result node)
       `((has-operand ,(value-of g node))
         ,@(reject-equations g node)
         ,@search-items)]
      [(g:no-neighbor-at-port? g 'source node)
       `((result (inexact ,(value-of g node)))
         ,@(reject-equations g node)
         ,@search-items)]
      [else search-items])))

(define (reject-equations g node)
  (for/list ([equation (tried-equations g node)])
    `(reject ,equation)))

(define (tried-equations g node)
  (for/list ([tag (tags-of g node)]
             #:when (node-is-a? g tag 'tried))
    (value-of g tag)))

(define (tag-touched-nodes-with-tried g archetype)
  (for/fold ([g g])
            ([node (g:graph-get-var g 'touched-nodes)])
    (add-tag g `(tried ,archetype) node)))

(define (new-blocks g)
  (for/list ([node (new-nodes g)]
             #:when (block? g node))
    node))

(define (block? g node)
  (eq? 'number (class-of g node)))

(define (target-of g ctx)
  (g:port->neighbor g `(,ctx target)))

(define (value-of-target g ctx)
  (value-of g (target-of g ctx)))

(define (any-new-block-is-target? g)
  (for/or ([new-block (new-blocks g)])
    (= (value-of g new-block) (value-of-target g 'ws))))

(define (new-operator g)
  (for/or ([node (new-nodes g)])
    (if (node-is-a? g node 'operator)
      node
      #f)))

(define (operands-of g node)
  (if node
    (g:port->neighbors g `(,node operands))
    '()))

(define (results-of g node)
  (if node
    (g:port->neighbors g `(,node result))
    '()))

(define (just-the-number nspec)
  (match nspec
    [(? number?) nspec]
    [`(inexact ,n) n]))

(define (result-searched-for search-items)
  (let loop ([search-items search-items])
    (match search-items
      ['() (void)]
      [`((result ,nspec) . ,more) (just-the-number nspec)]
      [`(,_ . ,more) (loop more)])))

(define (operands-searched-for search-items)
  (let loop ([search-items search-items])
    (match search-items
      ['() '()]
      [`((has-operand ,nspec) . ,more) (just-the-number nspec)]
      [`(,_ . ,more) (loop more)])))

(define (all-new-block-operands/result g relational-operator search-items)
  (match (result-searched-for search-items)
    [(? void?) 1.0]
    [result
      (for/fold ([x 1.0])
                ([operand (new-block-operands g)])
        (* x (if (relational-operator (value-of g operand) result)
                1.0
                0.8)))]))

(define (all-new-block-results/operands g relational-operator search-items)
  (for*/fold ([x 1.0])
            ([operand (operands-searched-for search-items)]
             [result-node (new-block-results g)])
    (* x (if (relational-operator (value-of g result-node) operand)
           1.0
           0.8))))

(define (new-block-operands g)
  (for/list ([node (operands-of g (new-operator g))]
             #:when (and (new-node? g node)
                         (block? g node)))
    node))

(define (new-block-results g)
  (for/list ([node (results-of g (new-operator g))]
             #:when (and (new-node? g node)
                         (block? g node)))
    node))

(define (promisingness g search-items)
  (case (class-of g (new-operator g))
    [(+ *)
     (cond
       [(any-new-block-is-target? g)
        -1.0]
       [else
         (*
           (all-new-block-operands/result g < search-items)
           (all-new-block-results/operands g > search-items))])]
    [(-)
     (cond
       [(any-new-block-is-target? g)
        -1.0]
       [else
         (*
           (all-new-block-operands/result g < search-items)
           (all-new-block-results/operands g > search-items))])]))


;; ======================================================================
;;
;; Running
;;

(define std-numbo-graph
  (let* ([g empty-numbo-graph]
         [g (add-memorized-equations g elementary-equation-exprs)]
         [g (add-tags-to-number-archetypes g)])
    g))

(define (do-timestep g)
  (with-handlers ([(eq?? 'nothing-to-do) (λ (_) (log "Nothing to do.") g)])
    (let*-values ([(g) (decay-saliences-in g 'ws)]
                  ;[focal-node (choose-focal-node g 'ws)]
;                  [(g focal-node) (look-for-problems g 'ws)]
;                  [(_) (log "focusing on" focal-node)]
;                  [(_) (pr-node g focal-node)]
;                  ;[(_) (pr-group g 'ws)]
                  [(search-items) (choose-search-items g 'ws)]
                  [(crawler) (make-crawler g 'equation search-items)]
                  [(crawler) (crawl-to-completion g crawler)]
                  [(equation) (crawler-found crawler)])
;                  [(initial-activations)
;                     (maybe-suspend 'slipnet-activations
;                                    (make-initial-activations g #;focal-node))]
;                  [(_) #R (sorted-by-cdr initial-activations)]
;                  [(g equation) (search-slipnet g initial-activations)]
    (if (void? equation)
      (start-over g)
      (try-equation g equation search-items)))))

(define (try-equation g equation search-items)
  (let*-values ([(_) (log "trying" equation)]
                [(g) (clear-touched-nodes g)]
                [(g) (clear-new-nodes g)]
                [(g) (complete-partial-instance-in g equation 'ws)]
                [(g) (remove-obsolete-tags g 'ws)]
                [(g) (boost-salience-of-touched-nodes g)]
                [(p) (promisingness g search-items)]
                [(_) (log "promisingness:" p)]
                [(g) (tag-touched-nodes-with-tried g equation)])
    g))

(define (start-over g)
  (log "giving up on that line of \"thought\"")
  (for/fold ([g g])
            ([node (blocks-and-operators g 'ws)])
    (g:remove-node g node)))

(define (blocks-and-operators g ctx)
  (for/list ([node (members-of g ctx)]
             #:when (or (block? g node) (node-is-a? g node 'operator)))
    node))

;Print the top activations
(define (top-as g [n 20])
  (take-right (sorted-by-cdr (g:graph-get-var g 'activations)) n))

(define (run^ g)
  (maybe-suspend 'g g)
  (with-handlers ([(λ (e) (match e
                            [`(done ,_) #t]
                            [else #f]))
                   (λ (e) (cadr e))])
    (for/fold ([g g])
              ([t max-timesteps])
      (if (done? g)
        (begin
          ;(log (result-expr g))
          (raise `(done ,g)))
        (maybe-suspend 'g (do-timestep g))))))

;TODO Pass slipnet as argument so you don't have to recreate it each time
(define (run bricks target)
    (run^ (start-numbo-ws std-numbo-graph bricks target)))

(define (runvoid bricks target)
  (set! g (run bricks target)))

(define (choose-focal-node g ctx)
  (safe-car
    (seq-weighted-by-salience g (members-of g 'ws))))

(define g (g:copy-graph std-numbo-graph))

;(gdo g:set-edge-weight '((1+1=2 activation) (archetype-has-operand-1
;                                              activation)) 2.0)

(gdo start-numbo-ws '(4 5 6) 15)
;(gdo start-numbo-ws '(0 4 5) 20)
;(gdo start-numbo-ws '(100 4 5) 20)
;(time (gdo do-timestep))

;(gdo start-numbo-ws '(3 8 4 6 1) 27)
;;(gdo complete-partial-instance-in '3*8=24 'ws)
;(define si (choose-search-items g 'ws))
;(define c (make-crawler g 'equation si))
;(define d (crawl-to-completion g c))
;(crawler-found d)

;(gdo start-numbo-ws '(1 1 1 1 1) 5)
(gdo do-timestep)