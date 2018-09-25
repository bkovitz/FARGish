#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         describe "graph.rkt")

(define (could-bind? g from-node to-node)
  (or (equal? (value-of g from-node) (value-of g to-node))
      (equal? (class-of g from-node) (class-of g to-node))))

(define (start-bdx-scout g from-ctx from-node to-ctx)
  `(:make
     (:define scout (:node bdx-scout))
     (:edge (scout from-node) (,from-node general-port))
     (:edge (scout from-ctx) (,from-ctx general-port))
     (:edge (scout to-ctx) (,to-ctx general-port))))

(define (run-bdx-scout g scout)
  (define from-node (port->neighbor g `(,scout from-node)))
  (define to-ctx (port->neighbor g `(,scout to-ctx)))
  (define to-node (for/first ([to-node (members-of g to-ctx)]
                              #:when (could-bind? g from-node to-node)
                              #:when (not (bound-to? g from-node to-node)))
                    to-node))
  (if to-node
    `(bind ,from-node ,to-node)  ;TODO another agent must build the edges
    `(:make
       (:define new-node (copy-node ,from-node ,to-ctx))
       (bind ,from-node new-node))))

(define (finish-archetype-instantiation g from-ctx to-ctx)
  (define deltas (for/list ([from-node (members-of g from-ctx)]
                             #:when (not (bound-from-ctx-to-ctx? g
                                           from-ctx to-ctx from-node)))
                   (start-bdx-scout g from-ctx from-node to-ctx)))
  `(:make ,@deltas))

(define (node->graph-delta g node)
  (match (class-of g node)
    ['bdx-scout
     (run-bdx-scout g node)]
    ['finish-archetype-instantiation
     (finish-archetype-instantiation g (port->neighbor g `(,node from-ctx))
                                       (port->neighbor g `(,node to-ctx)))]
    [_ #f]))

;NEXT Call this func and see if it works.
(define (do-timestep g)
  (define deltas (for/fold ([deltas '()])
                           ([node (all-nodes g)])
                   (let ([delta (node->graph-delta g node)])
                     (if delta (cons delta deltas) deltas))))
  #R deltas
  (do-graph-edits g deltas))

;(define g (make-graph 4))

;Need some sort ;of expr->graph func.
(define g (apply make-graph
  '((:group workspace
      4 5 6 15)
    (:group archetype
       4 5 + 9
       (:edge (4 result) (+ operands))
       (:edge (5 result) (+ operands))
       (:edge (+ result) (9 source)))
    (:make
      (:define finisher (:node finish-archetype-instantiation))
      (:edge (finisher from-ctx) (archetype general-port))
      (:edge (finisher to-ctx) (workspace general-port))))))

(pr-graph g)
