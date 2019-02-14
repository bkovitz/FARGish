; x32.rkt -- Experimenting with building up a graph to represent the temporal
;            trace for the solved "32" problem

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "model1.rkt"
         "support.rkt"
         "shorthand.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))

(define spec
  (farg-model-spec
    ; Nodes
    (nodeclass problem
      (is-a 'ctx))
    (nodeclass trace
      (is-a 'ctx))
    (nodeclass (t t)  ; a timestep in a trace
      (value t)
      (name (format "t~a" t))
      (is-a 'ctx))
    (nodeclass equation
      (is-a 'ctx))
    (nodeclass (number n)
      (value n)
      (name n))
    (nodeclass *)
    (nodeclass =)
    ; Tags
    (tagclass target
      (applies-to ([node as-tag])))
    (tagclass brick
      (applies-to ([node as-tag])))
    (tagclass greater-than
      (applies-to ([greater (of-class 'number)
                            (by-ports 'greater 'greater-than)]
                   [lesser (of-class 'number) (by-ports 'lesser 'less-than)])
        (condition (value-pred?/g > greater lesser))))
    (tagclass same
      (applies-to ([node1 as-tag] [node2 as-tag])
        (condition (and?/g (same-class?/g node1 node2)
                           (value-pred?/g equal? node1 node2)))))
    (tagclass (num-digits n)
      (value n)
      (name (format "num-digits-~a" n))
      (applies-to ([node (of-class 'number) as-tag])
        (condition (value-pred?/g
                     (λ (v) (safe-eqv? n (->num-digits v)))
                     node))))
    (tagclass consume
      (applies-to ([consumer (by-ports 'consumer 'consumes)]
                   [consumes (by-ports 'consumes 'consumed)])))

    ;(chain equation prev -> next ...)
    ))

;; ======================================================================
;;
;; Utility functions
;;

(define (->num-digits x)
  (safe-string-length (safe-integer->string x)))

;TODO Look up appropriate port names from tag's nodeclass
;Better yet, delete this function and add support automatically in make-tag
(define (tag->edges g tag node)
  `((,tag tagged) (,node tags)))

(define (node->num-digits g node)
  (->num-digits (value-of g node)))

;; ======================================================================
;;
;; Quick matches
;;

(define (add-tag-support g tag node)
  (let*-values ([(e) (tag->edges g tag node)]
                [(g) (add-mutual-support g node e)]
                [(g) (add-mutual-support g tag e)])
    g))

; Graph Node -> (List Action)
(define (qm/num-digits g node)
  (let ([nd (node->num-digits g node)])
    (cond
      [(void? nd) '()]
      [else
        (list (λ (g)
          (let*-values ([(g tag) (make-tag g `(num-digits ,nd) node)]
                        [(g) (add-tag-support g tag node)])
            g)))])))

(define (qm/same g node1)
  (let* ([fi (λ (node2)
               (tagclass-applies-to? g 'same node1 node2))]
         [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
    (if node2
       (list (λ (g)
               (let*-values ([(g tag) (make-tag g 'same node1 node2)]
                             [(g) (add-tag-support g tag node1)]
                             [(g) (add-tag-support g tag node2)])
                 g)))
       '())))

;; ======================================================================
;;
;; The solution to the "32" problem, as a stored temporal trace
;;

(define g
  (let*-values
    ([(g) (make-empty-graph spec)]
     ;temporal trace
     [(g trace) (make-node g 'trace)]
     ;t0
     [(g t0) (make-node/in g trace 't 0)]
     ; problem
     [(g problem) (make-node/in g t0 'problem)]
     [(g target) (make-node/in g 'problem 'number 32)]
     [(g) (add-tag g 'target target)]
     [(g b6) (make-node/in g 'problem 'number 6)]
     [(g) (add-tag g 'brick b6)]
     [(g b3) (make-node/in g 'problem 'number 3)]
     [(g) (add-tag g 'brick b3)]
     [(g b4) (make-node/in g 'problem 'number 4)]
     [(g) (add-tag g 'brick b4)]
     [(g b5) (make-node/in g 'problem 'number 5)]
     [(g) (add-tag g 'brick b5)]

     ;t1
     [(g t1) (make-node/in g trace 't 1)]
     [(g) (add-edge g `((,t0 next) (,t1 prev)))]
     [(g eqn1) (make-node/in g t1 'equation)]
     [(g e6) (make-node/in g eqn1 'number 6)]
     [(g c1) (make-tag g 'consume e6 b6)]
     [(g times) (make-node/in g eqn1 '*)]
     [(g) (add-edge g `((,e6 next) (,times prev)))]
     [(g e4) (make-node/in g eqn1 'number 4)]
     [(g c2) (make-tag g 'consume e4 b4)]
     [(g) (add-edge g `((,times next) (,e4 prev)))]
     [(g equals1) (make-node/in g eqn1 '=)]
     [(g) (add-edge g `((,e4 next) (,equals1 prev)))]
     [(g e24) (make-node/in g eqn1 'number 24)]
     [(g) (add-edge g `((,equals1 next) (,e24 prev)))]

     [(g) (make-permanent g trace
                          t0 problem target b6 b3 b4 b5 c1 c2
                          t1 eqn1 e6 times e4 equals1 e24)]
     )
    g))

(set! g g)

(define a (qm/num-digits g 24))
(gdo (car a))
(define b (qm/same g 6))
(gdo (car b))
(gdo support->t+1)

(pr-graph g)
