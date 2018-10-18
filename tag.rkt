;; tag.rkt -- Generic code for tags

#lang debug at-exp racket

(require "graph.rkt" "makegraph.rkt")
(require racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

(struct tagspec (name add-tag target-nodes conditions extends mates) #:prefab)

(define need-source
  (tagspec
    '(need source)
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

;(define (add-tag g tagspec . nodes)

(define (taggees-of g tagnode)
  (port->neighbors g `(,tagnode tagged)))
