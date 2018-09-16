#lang debug at-exp racket

(require data/collection racket/pretty "graph.rkt")

(define (bind-all-letters g)
  (for/fold ([g g])
            ([letter-pair '((a b) (b a) (a c) (c a) (b c) (c b))])
    (match-define `(,from ,to) letter-pair)
    (add-tag g 'bind from to)))

#;(define model
  (let* ([g (make-graph 'a 'b 'c '(tag next a b) '(tag next b c))]
         [g (bind-all-letters g)])
    g))

(define model
  (let* ([g (make-graph '(group workspace
                                a (placeholder x letter) c
                                (bind a x) (bind x c))
                        '(group slipnet-structure
                                a b c (succ a b) (succ b c)))])
    g))

(pr-graph model)

;TODO UT
(define (try-to-bind-all g from-group to-group)
  (for/fold ([g g])
            ([from-node (members-of g from-group)])
    (define class (class-of g from-node))
    (define to-nodes (nodes-of-class-in g class to-group))
    (if (empty? to-nodes)
        (let*-values ([(g newid) (make-node g `((class . ,class)))]
                      [(g) (add-edge g `((,to-group members)
                                         (,newid member-of)))])
          (bind g from-node newid))
        (for/fold ([g g])
                  ([to-node to-nodes])
          (bind g from-node to-node)))))

(printf "\n")

(pr-graph (try-to-bind-all model 'slipnet-structure 'workspace))
