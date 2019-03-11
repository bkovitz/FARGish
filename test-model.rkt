; test-model.rkt -- Unit tests for model.rkt

#lang debug typed/racket
(require typed/debug/report)

(module+ test
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

    (check-equal? (list->set (all-nodes g))
                  (set ws number22 brick7))

    ; display-names
    (check-equal? (display-name-of g number22) 22)
    (check-equal? (display-name-of g brick7) 7)

    ; values
    (check-equal? (value-of g number22) 22)
    (check-equal? (value-of g brick7) 7)

    ; linking into a ctx
    (check-equal? (port->neighboring-ports g `(,ws members))
                  (set `(,number22 member-of) `(,brick7 member-of)))

    ; is-a ancestor relations
    (check-true (node-is-a? g brick7 'brick))
    (check-true (node-is-a? g brick7 'number))
    (check-true (node-is-a? g number22 'number))
    (check-false (node-is-a? g number22 'brick))
    ))
