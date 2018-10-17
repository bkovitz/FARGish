;; tag.rkt -- Generic code for tags

#lang debug at-exp racket

(require "graph.rkt" "makegraph.rkt")
(require racket/hash)
(require rackunit racket/pretty describe)

(struct tagspec (add-tag target-nodes conditions extends mates) #:transparent)

(define need-source
  (tagspec
    ;add-tag
    (λ (node)
      `(:let ([:tag (:node ,(tag->attrs tag))])
          (:edge (:tag tagged) (,node tags))))
    ;target-nodes
    '((node number))
    ;conditions
    `((,(λ (g node)
          (empty? (port->neighbors g `(,node source))))))
    ;extends
    '(problem)
    ;mates
    (hash 'solution (λ (g this node)
                      `(fills-port ,(value-of g node) source)))))

