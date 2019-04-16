; x32.rkt -- Experimenting with building up a graph to represent the temporal
;            trace for the solved "32" problem

#lang debug at-exp typed/racket
(require typed/debug/report)

(require errortrace)
(require typed/json)
(require "typed-wheel.rkt")
(require "types.rkt" "fargish.rkt" "model.rkt" "trace.rkt")

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
    (display-name (format "t~a" t)))
  (nodeclass equation)
  (nodeclass (number [n : Integer])
    (value n)
    (display-name n))
  ;(nodeclass placeholder)
  (nodeclass times
    (display-name "*"))
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

(: tag-and-support/ : Attrs Node * -> Action)
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
;; Local matches
;;

(define-type Action (-> Graph Graph))

(struct LocalMatch
  ([g->node1-candidates : (Graph (U Node Void) -> (Listof Node))]
   [node1->actions : (Graph Node -> (Listof Action))])
  #:prefab)

(: lm->candidates : Graph LocalMatch (U Node Void) -> (Listof Node))
(define (lm->candidates g lm start-node)
  ((LocalMatch-g->node1-candidates lm) g start-node))

(: lm+node1->actions : Graph LocalMatch Node -> (Listof Action))
(define (lm+node1->actions g lm node1)
  ((LocalMatch-node1->actions lm) g node1))

(define lm/fill
  (LocalMatch
    (λ ([g : Graph] _) (filter/g g node-is-placeholder? (all-nodes g)))
    (λ ([g : Graph] [node-to-fill : Node])
      (let ([fi (could-fill?/ g node-to-fill)]
            [filler-node (choose-nearby-node-by-salience
                           g node-to-fill #:filter fi)])
        (cond
          [(not (void? filler-node))
           (list (tag-and-support/ (fill) node-to-fill filler-node))]
          [else '()])))))

(define lm/greater-than
  (LocalMatch
    (λ ([g : Graph] _) (filter/g g node-is-a-number? (all-nodes g)))
    (λ ([g : Graph] [node1 : Node])
      (let ([fi (λ ([node2 : Node])
                  (tagclass-applies-to? g 'greater-than (list node1 node2)))]
            [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
        (cond
          [(void? node2) '()]
          [else (list (tag-and-support/ (greater-than) node1 node2))])))))

(define lm/same
  (LocalMatch
    (λ ([g : Graph] _) (all-nodes g))
    (λ ([g : Graph] [node1 : Node])
      (let ([fi (λ ([node2 : Node])
                  (and (not (eq? node1 node2))
                       (tagclass-applies-to? g 'same (list node1 node2))))]
            [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
        (cond
          [(void? node2) '()]
          [else (list (tag-and-support/ (same) node1 node2))])))))

(define lms (list lm/fill lm/greater-than lm/same))

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
    [else (lm+node1->actions g lm node1)]))

(: do-local-matches : Graph -> Graph)
(define (do-local-matches g)
  (let ([actions (for/fold ([actions : (Listof Action) '()])
                           ([i 6])
                   (append (lm->actions g (random-lm))))])
    (for/fold ([g g])
              ([action actions])
      (action g))))

;; ======================================================================
;;
;; The solution to the "32" problem, as a stored temporal trace
;;

(define g
  (let
    ([(g) (make-empty-graph spec)]
     ;old temporal trace
     [(g trace) (make-node g (trace))]
     ;t0
     [(g t0) (make-node/in g trace (t 0))]
     [(g) (add-edge g `((,trace first) (,t0 first-in)))]
     ; problem
     [(g problem) (make-node/in g t0 'problem)]
     [(g target) (make-node/in g problem (number 32))]
     [(g) (add-tag g 'target target)]
     [(g b6) (make-node/in g problem (number 6))]
     [(g) (add-tag g 'brick b6)]
     [(g b3) (make-node/in g 'problem (number 3))]
     [(g) (add-tag g 'brick b3)]
     [(g b4) (make-node/in g 'problem (number 4))]
     [(g) (add-tag g 'brick b4)]
     [(g b5) (make-node/in g 'problem (number 5))]
     [(g) (add-tag g 'brick b5)]

     ;t1
     [(g t1) (make-node/in g trace (t 1))]
     [(g) (add-edge g `((,t0 next) (,t1 prev)))]
     [(g eqn1) (make-node/in g t1 'equation)]
     [(g e6) (make-node/in g eqn1 (number 6))]
     [(g) (add-edge g `((,eqn1 first) (,e6 first-in)))]
     [(g c1) (make-tag g 'consume (list e6 b6))]
     [(g times) (make-node/in g eqn1 'times)]
     [(g) (add-edge g `((,e6 next) (,times prev)))]
     [(g e4) (make-node/in g eqn1 (number 4))]
     [(g c2) (make-tag g 'consume (list e4 b4))]
     [(g) (add-edge g `((,times next) (,e4 prev)))]
     [(g equals1) (make-node/in g eqn1 'equals)]
     [(g) (add-edge g `((,e4 next) (,equals1 prev)))]
     [(g e24) (make-node/in g eqn1 (number 24))]
     [(g) (add-edge g `((,equals1 next) (,e24 prev)))]

;     [(g) (make-permanent g trace
;                          t0 problem target b6 b3 b4 b5 c1 c2
;                          t1 eqn1 e6 times e4 equals1 e24)]
     )
    g))

(set! g g)

(pr-graph g)

(write-json (graph->jsexpr g))
