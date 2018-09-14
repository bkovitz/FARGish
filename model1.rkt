#lang racket

(require racket/pretty "graph.rkt")

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
  (let* ([g (make-graph 'a '(placeholder x letter) 'c
                        '(bind a x) '(bind x c))])
    g))

(pr-graph model)
