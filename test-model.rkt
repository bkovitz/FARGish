; test-model.rkt -- Unit tests for model.rkt

#lang typed/racket

(require "model.rkt"
         "fargish.rkt"
         typed/rackunit)

(test-case "spec basics"
  (define-spec spec
    (nodeclass workspace)
    (nodeclass (number [n : Integer])
      (value n)
      (display-name n))
    (nodeclass (brick [n : Integer])
      (is-a number)))

  (define g (make-empty-graph spec))

  (define ws (gdo make-node (workspace)))
  (define number22 (gdo make-node/in ws (number 22)))
  (define brick7 (gdo make-node/in ws (brick 7)))

  (pr-graph g)
  )
