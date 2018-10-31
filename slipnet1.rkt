; slipnet1.rkt -- Code for making and querying a slipnet

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "fargish.rkt" (prefix-in g: "graph1.rkt")
(require rackunit racket/pretty describe)

;; ======================================================================
;;
;; Making a slipnet
;; 

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
  (define spec
    (farg-model-spec
      (nodeclass ws
        (is-a 'ctx)) ; doesn't have an archetype
      (nodeclass number
        (is-a 'ctx)
        (archetype is-value))
      (nodeclass operator
        (archetype is-class))
      (nodeclass +
        (is-a 'operator))
      (nodeclass equation
        (is-a 'ctx)
        (archetype is-node))))

  ...)
