#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         describe "graph.rkt")

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
  (let* ([g (make-graph '(:group workspace
                                 a (placeholder letter x) c
                                 (bind a x) (bind x c))
                        '(:group slipnet-structure
                                 a b c (succ a b) (succ b c)))])
    g))

#;(pr-graph model)

;TODO UT
(define (try-to-bind-all g from-group to-group)
  (do-graph-edits g
    (for/list ([from-node (members-of g from-group)])
      (define class (class-of g from-node))
      (define to-nodes (nodes-of-class-in g class to-group))
      (if (null? to-nodes) ;if nothing to bind to, then build
        (let ([new (gensym class)])
          `(:make
             (:define ,new (:node ,class)) ;BUG? :node redefines succ
             (:edge (,to-group members) (,new member-of))
             (bind ,from-node ,new)))
        `(:make  ;bind to all of same class
           ,@(for/list ([to-node to-nodes])
               `(bind ,from-node ,to-node)))))))
;
;
;
;
;
;  (for/fold ([g g])
;            ([from-node (members-of g from-group)])
;    (define class (class-of g from-node))
;    (define to-nodes (nodes-of-class-in g class to-group))
;    (if (empty? to-nodes)
;        (let*-values ([(g newid) (make-node g `((class . ,class)))]
;                      [(g) (add-edge g `((,to-group members)
;                                         (,newid member-of)))])
;          (bind g from-node newid))
;        (for/fold ([g g])
;                  ([to-node to-nodes])
;          (bind g from-node to-node)))))

(newline)

;(pr-graph (try-to-bind-all model 'slipnet-structure 'workspace))

(module+ test
  (test-case "try-to-bind-all"
    (let* ([g (make-graph '(:group workspace
                                   a (placeholder letter x) c
                                   (bind a x) (bind x c))
                          '(:group slipnet-structure
                                   a b c (succ a b) (succ b c)))]
           [g (try-to-bind-all g 'slipnet-structure 'workspace)])
      (check-equal? (list->set (member-of g 'succ3)) (set 'workspace))
      (check-equal? (list->set (member-of g 'succ4)) (set 'workspace))
      (check-true (bound-to? g 'a2 'a))
      (check-true (bound-to? g 'a2 'x))
      (check-true (bound-to? g 'a2 'c))
      (check-true (bound-to? g 'b 'a))
      (check-true (bound-to? g 'b 'x))
      (check-true (bound-to? g 'b 'c))
      (check-true (bound-to? g 'c2 'a))
      (check-true (bound-to? g 'c2 'x))
      (check-true (bound-to? g 'c2 'c))
      #;(check-true (bound-to? g #R(tag-of 'succ g 'a2 'b)
                               #R(tag-of 'succ g 'a 'x)))
      (check-true (bound-to? g 'succ2 'succ3))
      (check-true (bound-to? g 'succ 'succ4))
    )))


(define g (make-graph '(:group workspace a b)
                      '(:group slipnet-structure a b (succ a b))))

