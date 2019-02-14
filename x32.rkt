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
    (nodeclass placeholder)
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
  (cond
    [(not (node-is-a? g node 'number)) '()]
    #:define nd (node->num-digits g node)
    [(void? nd) '()]
    [else (list (λ (g)
            (let*-values ([(g tag) (make-tag g `(num-digits ,nd) node)]
                          [(g) (add-tag-support g tag node)])
              g)))]))

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

(define (qm/greater-than g node1)
  (let* ([fi (λ (node2)
               (tagclass-applies-to? g 'greater-than node1 node2))]
         [node2 (choose-nearby-node-by-salience g node1 #:filter fi)])
    (if node2
       (list (λ (g)
               (let*-values ([(g tag) (make-tag g 'greater-than node1 node2)]
                             [(g) (add-tag-support g tag node1)]
                             [(g) (add-tag-support g tag node2)])
                 g)))
       '())))

(define qms (list qm/num-digits qm/same qm/greater-than))

; Very crude version. TODO: Choose qms via slipnet, or something to reflect
; bias toward qms that attract attention right now. The code could be a lot
; more efficient, too.
(define (quick-match g)
  (let* ([node->salience (g->node->salience g)]
         [random-qm (let ([len (length qms)])
                      (λ ()
                        (list-ref qms (random len))))]
         [nodes (for/list ([i 6])
                  (weighted-choice-by node->salience (all-nodes g)))]
         [actions (for*/list ([node nodes]
                              [action ((random-qm) g node)])
                    action)]
         [g (for/fold ([g g])
                      ([action actions])
              (action g))])
    g))

(define (step g)
  (let* ([g (quick-match g)]
         [g (support->t+1 g)])
    g))

;; ======================================================================
;;
;; "Copying" a temporal trace
;;

; TODO This should be done by quick-matching, not hard-coded.
(define (copy-trace g trace)
  (let*-values ([(g new-trace) (make-node g 'trace)]
                [(g) (mark-copying g trace new-trace)]
                [(g) (let loop ([g g] [t (port->neighbor g `(,trace first))])
                       (cond
                         [(void? t) g]
                         [else (loop (copy-t g t new-trace)
                                     (port->neighbor g `(,t next)))]))])
    (values g new-trace)))

(define (copy-t g t into-trace)
  (let*-values ([(g new-t) (make-node/in g into-trace 't (value-of g t))]
                [(g) (mark-copying g t new-t)]
                [(g) (add-parallel-edge g new-t t 'prev)])
    g))

(define (mark-copying g from-node to-node)
  (add-edge g `((,from-node copying-to) (,to-node copying-from))))

(define (copying-from g node)
  (port->neighbor g `(,node copying-from)))

(define (copying-to g node)
  (port->neighbor g `(,node copying-to)))

(define (add-parallel-edge g new-node old-node port-label)
  (cond
    #:define old-nbr-port (port->neighboring-port g `(,old-node ,port-label))
    [(void? #R old-nbr-port) g]
    #:match-define `(,old-nbr ,old-nbr-port-label) old-nbr-port
    #:define new-nbr (copying-to g old-nbr)
    [(void? #R new-nbr) g]
    [else (add-edge g `((,new-nbr ,old-nbr-port-label)
                        (,new-node ,port-label)))]))

(define (copy-members g old-ctx new-ctx)
  (for/fold ([g g] #:result (copy-ordering-edges g old-ctx new-ctx))
            ([old-member (members-of g old-ctx)])
    (let*-values ([(g new-member) (apply make-node/in g new-ctx
                                                        (class-of g old-member)
                                                        (args-of g old-member))]
                  [(g) (mark-copying g old-member new-member)])
      g)))

;TODO first-in
(define (copy-ordering-edges g old-ctx new-ctx)
  (for/fold ([g g])
            ([new-member (members-of g new-ctx)])
    (cond
      #:define old-member #R (copying-from g new-member)
      [(void? old-member) g]
      [else (add-parallel-edge g new-member old-member 'next)])))

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
     [(g) (add-edge g `((,trace first) (,t0 first-in)))]
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
     [(g) (add-edge g `((,eqn1 first) (,e6 first-in)))]
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

;(define a (qm/num-digits g 24))
;(gdo (car a))
;(define b (qm/same g 6))
;(gdo (car b))
;(define c (qm/greater-than g 4))
;(gdo (car c))

;(gdo quick-match)
;(gdo support->t+1)

(gdo step)
(gdo step)
(gdo copy-trace 'trace)
(gdo copy-members 'equation 'equation2)

(pr-graph g)
