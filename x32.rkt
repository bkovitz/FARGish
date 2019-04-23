; x32.rkt -- Experimenting with building up a graph to represent the temporal
;            trace for the solved "32" problem

#lang debug at-exp typed/racket
(require typed/debug/report)

(require errortrace)
(require typed/json)
(require "typed-wheel.rkt")
(require "types.rkt" "fargish.rkt" "model.rkt" "trace.rkt" "support-typed.rkt")

(provide step/web write-graph/json)

;(: ->num-digits : (U Integer Void) -> (U Integer Void))
;(define (->num-digits x)
;  (safe-string-length (safe-integer->string x)))

(: ->num-digits : Any -> (U Integer Void))
(define (->num-digits x)
  (cond
    [(integer? x) (safe-string-length (safe-integer->string x))]
    [else (void)]))

(define-spec spec
  ; Nodes
  (nodeclass problem)
  (nodeclass trace)
  (nodeclass (t [t : Integer])  ; a timestep in a trace
    (value t)
    (display-name (format "t~a" t)))  ;TODO Why is the 't' omitted in the
                                      ;actual display-name that comes out?
  (nodeclass equation)
  (nodeclass (number [n : Integer])
    (value n)
    (display-name n))
  ;(nodeclass placeholder)
  (nodeclass times
    (display-name "×"))
  (nodeclass equals
    (display-name "="))
  ; Tags
  (tagclass target
    (applies-to ([node])))
  (tagclass brick
    (applies-to ([node])))
  (tagclass greater-than
    (applies-to ([greater (of-class number)
                          (by-ports greater greater-than)]
                 [lesser (of-class number) (by-ports lesser less-than)])
      (condition (safe->? (value-of g greater) (value-of g lesser)))))
  (tagclass same
    (applies-to ([node1] [node2]) ;TODO not same node
      (condition (and (same-class? g node1 node2)
                      (safe-equal? (value-of g node1)
                                   (value-of g node2))))))
  (tagclass (pow10 [p : Integer])  ; taggee is a power of 10 (but not 1)
    (value p)
    (display-name (format "pow10 ~a" p))
    (applies-to ([node])
      (condition (let ([taggee-p (pow10-of (value-of g node))])
                   (and (not (void taggee-p))
                        (not (= 0 taggee-p)))))))

  (tagclass (num-digits [n : Integer])
    (value n)
    (display-name (format "num-digits-~a" n))
    (applies-to ([node (of-class number)])
      (condition (safe-eqv? (value-of g this)
                            (->num-digits (value-of g node))))))
  (tagclass fill
    (applies-to ([node-to-fill (by-ports fill filled-by)]
                 ;TODO Make sure node-to-fill can be filled
                 [filler-node (by-ports filled-by fill)])))
  (tagclass can-be-consumed
    (applies-to ([node])))
  (tagclass consume
    (is-a fill)
    (applies-to ([consumer (by-ports consumer filled-by)]
                 [consumes (by-ports consumes fill)])))

  ;(chain equation prev -> next ...)
  )

;; ======================================================================
;;
;; Utility functions
;;

;TODO Look up appropriate port names from tag's nodeclass
;Better yet, delete this function and add support automatically in make-tag
(: tag->edges : Graph Node Node -> Hop)
(define (tag->edges g tag node)
  `((,tag tagged) (,node tags)))

(: tag-and-support : Graph Attrs Node * -> Graph)
(define (tag-and-support g tagspec . nodes)
  (let ([(g tag) (make-tag g tagspec nodes)]
        [g (for/fold ([g : Graph g])
                     ([node nodes])
             (add-tag-support g tag node))])
    g))

(: tag-and-support/ : Attrs Node * -> (-> Graph Graph))
(define (tag-and-support/ tagspec . nodes)
  (λ (g) (apply tag-and-support g tagspec nodes)))

;TODO Write TR version of add-mutual-support
(: add-tag-support : Graph Node Node -> Graph)
(define (add-tag-support g tag node)
  (let ([e (tag->edges g tag node)]
        #;[g (add-mutual-support g node e)]
        #;[g (add-mutual-support g tag e)])
    g))

(: node-is-placeholder? : Graph Node -> Boolean)
(define (node-is-placeholder? g node)
  (and (node-attr? g node 'placeholder?)
       (void? (value-of g node))))

(: node-is-a-number? : Graph MaybeNode -> Boolean)
(define (node-is-a-number? g node)
  (and (node-is-a? g node 'number)
       (has-value? g node)))

(: all-numbers : Graph -> (Listof Node))
(define (all-numbers g)
  (filter/g g node-is-a-number? (all-nodes g)))

; Returns void if n is not a power of 10.
(: pow10-of : Any -> (U Integer Void))
(define (pow10-of n)
  (cond
    [(not (integer? n)) (void)]
    [else (let loop ([n : Integer (if (negative? n) (- n) n)] [p 0])
            (cond
              [(= n 1) p]
              [(zero? n) (void)]
              #:define n (/ n 10)
              [(integer? n) (loop n (add1 p))]
              [else (void)]))]))

(: same-class? : Graph MaybeNode MaybeNode -> Boolean)
(define (same-class? g node1 node2)
  (safe-equal? (class-of g node1) (class-of g node2)))

; TODO Better class-matching. E.g. * should be able to fill +.
(: could-fill? : Graph Node Node -> Boolean)
(define (could-fill? g node-to-fill filler-node)
  (and (not (void? (value-of g filler-node)))
       (node-is-a? g filler-node (class-of g node-to-fill))))

(: could-fill?/ : Graph Node -> (-> Node Boolean))
(define (could-fill?/ g node-to-fill)
  (λ (filler-node)
    (could-fill? g node-to-fill filler-node)))

;; ======================================================================
;;
;; Problems (that is, numbles)
;;

(: make-numble : Graph Integer Integer * -> (Values Graph Node))
(define (make-numble g targ . bricks)
  (let ([(g numble) (make-node g (problem))]
        [(g target-node) (make-node/in g numble (number targ))]
        [g (add-tag g (target) target-node)]
        [g : Graph (make-permanent g numble target-node)]
        [g (for/fold ([g : Graph g])
                     ([b (in-list bricks)])
             (let ([(g brick-node) (make-node/in g numble (number b))]
                   [g (add-tag g (brick) brick-node)]
                   [g (make-permanent g brick-node)]
                   [g (add-tag g (can-be-consumed) brick-node)])
               g))])
    (values g numble)))

;; ======================================================================
;;
;; Local matches
;;

;(define-type Action (-> Graph Graph))

(define-struct/exec Action ([name : Any] [f : (Graph -> Graph)])
  [(λ ([a : Action] [g : Graph])
     ((Action-f a) g)) : (Action Graph -> Graph)])

(struct LocalMatch
  ([g->node1-candidates : (Graph (U Node Void) -> (Listof Node))]
   [node1->action : (Graph Node -> (U Action Void))])
  #:prefab)

(: lm->candidates : Graph LocalMatch (U Node Void) -> (Listof Node))
(define (lm->candidates g lm start-node)
  ((LocalMatch-g->node1-candidates lm) g start-node))

(: lm+node1->action : Graph LocalMatch Node -> (U Action Void))
(define (lm+node1->action g lm node1)
  ((LocalMatch-node1->action lm) g node1))

(define lm/fill
  (LocalMatch
    (λ ([g : Graph] _) (filter/g g node-is-placeholder? (all-nodes g)))
    (λ ([g : Graph] [node-to-fill : Node])
      (let ([fi (could-fill?/ g node-to-fill)]
            [filler-node (choose-nearby-node-by-salience
                           g node-to-fill #:filter fi)])
        (cond
          [(not (void? filler-node))
           (Action @~a{fill @node-to-fill with @filler-node}
                   (tag-and-support/ (fill) node-to-fill filler-node))]
          [else (void)])))))

(define lm/greater-than
  (LocalMatch
    (λ ([g : Graph] _) (all-numbers g))
    (λ ([g : Graph] [node1 : Node])
      (let ([fi (λ ([node2 : Node])
                  (and
                    (not (has-tag? g 'greater-than (list node1 node2)))
                    (tagclass-applies-to? g 'greater-than (list node1 node2))))]
            [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
        (cond
          [(void? node2) (void)]
          [else (Action @~a{greater-than @node1 @node2}
                        (tag-and-support/ (greater-than) node1 node2))])))))

(define lm/same
  (LocalMatch
    (λ ([g : Graph] _) (all-nodes g))
    (λ ([g : Graph] [node1 : Node])
      (let ([fi (λ ([node2 : Node])
                  (and (not (eq? node1 node2))
                       (not (has-tag? g 'same (list node1 node2)))
                       (tagclass-applies-to? g 'same (list node1 node2))))]
            [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
        (cond
          [(void? node2) (void)]
          [else (Action @~a{same @node1 @node2}
                        (tag-and-support/ (same) node1 node2))])))))

(define lm/pow10
  (LocalMatch
    (let ([ok? (λ ([g : Graph] [node : Node])
                 (and (node-is-a-number? g node)
                      (not (has-tag? g 'pow10 node))))])
      (λ ([g : Graph] _) (filter/g g ok? (all-nodes g))))
    (λ ([g : Graph] [node : Node])
      (let ([p (pow10-of (value-of g node))])
        (cond
          [(void? p) (void)]
          [else (Action @~a{pow10 @node @p}
                        (tag-and-support/ (pow10 p) node))])))))

(: node->num-digits : Graph Node -> (U Integer Void))
(define (node->num-digits g node)
  (->num-digits (value-of g node)))

(define lm/num-digits
  (LocalMatch
    (let ([ok? (λ ([g : Graph] [node : Node])
                 (and (node-is-a-number? g node)
                      (not (has-tag? g 'num-digits node))))])
      (λ ([g : Graph] _) (filter/g g ok? (all-nodes g))))
    (λ ([g : Graph] [node : Node])
      (cond
        #:define nd (node->num-digits g node)
        [(void? nd) (void)]
        [else (Action @~a{num-digits @nd @node}
                      (tag-and-support/ (num-digits nd) node))]))))

(define lms (list lm/fill lm/greater-than lm/same lm/num-digits lm/pow10))

(: random-lm : -> LocalMatch)
(define random-lm
  (let ([len (length lms)])
    (λ ()
      (list-ref lms (random len)))))

(: lm->actions : Graph LocalMatch -> (Listof Action))
(define (lm->actions g lm)
  (cond
    #:define node1 (weighted-choice-by
                     (λ ([node : Node]) (salience-of g node))
                     (lm->candidates g lm (void)))
    [(void? node1) '()]
    #:define a (lm+node1->action g lm node1)
    [(void? a) '()]
    [else (list a)]))

(: do-local-matches : Graph -> Graph)
(define (do-local-matches g)
  (let ([actions (for/fold ([actions : (Listof Action) '()])
                           ([i 6])
                   (append actions (lm->actions g (random-lm))))])
    (for/fold ([g g])
              ([action actions])
      (displayln (Action-name action))
      (action g))))

;; ======================================================================
;;
;; The solution to the "32" problem, as a stored temporal trace
;;

(define init-g : Graph
  (let
    ([(g) (make-empty-graph spec)]
     ;old temporal trace
     [(g trace) (make-node g (trace))]
     [g (set-node-attr g trace 'display-name "memory trace")]
     ;t0
     [(g t0) (make-node/in g trace (t 0))]
     [(g) (add-edge g `((,trace first) (,t0 first-in)))]
     ; problem
     [(g problem) (make-node/in g t0 'problem)]
     [(g targ) (make-node/in g problem (number 32))]
     [(g) (add-tag g (target) targ)]
     [(g b6) (make-node/in g problem (number 6))]
     [(g) (add-tag g (brick) b6)]
     [(g b3) (make-node/in g 'problem (number 3))]
     [(g) (add-tag g (brick) b3)]
     [(g b4) (make-node/in g 'problem (number 4))]
     [(g) (add-tag g (brick) b4)]
     [(g b5) (make-node/in g 'problem (number 5))]
     [(g) (add-tag g (brick) b5)]

     ;t1
     [(g t1) (make-node/in g trace (t 1))]
     [(g) (add-edge g `((,t0 next) (,t1 prev)))]
     [(g eqn1) (make-node/in g t1 (equation))]
     [(g e6) (make-node/in g eqn1 (number 6))]
     [(g) (add-edge g `((,eqn1 first) (,e6 first-in)))]
     [(g c1) (make-tag g (consume) (list e6 b6))]
     [(g times) (make-node/in g eqn1 (times))]
     [(g) (add-edge g `((,e6 next) (,times prev)))]
     [(g e4) (make-node/in g eqn1 (number 4))]
     [(g c2) (make-tag g (consume) (list e4 b4))]
     [(g) (add-edge g `((,times next) (,e4 prev)))]
     [(g equals1) (make-node/in g eqn1 (equals))]
     [(g) (add-edge g `((,e4 next) (,equals1 prev)))]
     [(g e24) (make-node/in g eqn1 (number 24))]
     [(g) (add-edge g `((,equals1 next) (,e24 prev)))]

;     [(g) (make-permanent g trace
;                          t0 problem target b6 b3 b4 b5 c1 c2
;                          t1 eqn1 e6 times e4 equals1 e24)]
;     [g (set-salience-of g (all-nodes g) 1.0)]
     )
    g))

(define g : Graph init-g)
(set! g g)

;(pr-graph g)
;
;(write-json (graph->jsexpr g))

(: write-graph/json (->* [Graph] [Output-Port] Any))
(define (write-graph/json g [output-port (current-output-port)])
  (write-json (graph->d3 g) output-port))

(: step : Graph -> Graph)
(define (step g)
  (let ([g (bump-t g)]
        [(g) (clear-touched-nodes g)]
        [(g) (clear-new-nodes g)]
        [g (decay-salience/all g)]
        [g (if (= 1 (current-t g))
             (let ([(g new-trace) (copy-trace g 'trace)]
                   [g (set-node-attr g new-trace 'display-name "live trace")]
                   [(g new-numble) (make-numble g 100 9 10 7 3)]
                   [g (make-member-of g (path->node g new-trace 'first)
                                        new-numble)]
                   #;[g (set-salience-of g (all-nodes g) 1.0)]) ;HACK
               g)
             g)]
        [g (do-local-matches g)]
        [g (boost-salience-of-touched-nodes g)])
    #R (length (all-nodes g))
    g))

(: step/web : (U Graph Void) -> Graph)
(define (step/web g)
  (cond
    [(void? g) init-g]
    [else (step g)]))

(: step! : -> Void)
(define (step!)
  (set! g (step g)))
