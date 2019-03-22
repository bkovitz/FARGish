; test-model.rkt -- Unit tests for model.rkt

#lang debug typed/racket
(require typed/debug/report)

(module+ test
  (require "model.rkt"
           "fargish.rkt"
           "typed-wheel.rkt"
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
    (check-false (node-is-a? g number22 'brick)))

  (test-case "members-of/rec"
    (define-spec spec
      (nodeclass container))
    (let ([g (make-empty-graph spec)]
          [(g c1) (make-node g (container))]
          [(g c2) (make-node/in g c1 (container))]
          [(g c3) (make-node/in g c1 (container))]
          [(_) (check-equal? (members-of/rec g c1)
                             (set c2 c3))]
          [(g c4) (make-node/in g c2 (container))]
          [(_) (check-equal? (members-of/rec g c1)
                             (set c2 c3 c4))]
          [(_) (check-equal? (members-of/rec g c2)
                             (set c4))]
          [(_) (check-equal? (members-of/rec g c3)
                             (set))])
      (void)))
  
  (test-case "filter/g"
    (define-spec spec
      (nodeclass (number [n : Integer])
        (display-name n)
        (value n))
      (tagclass a-tag))
    (let ([g (make-empty-graph spec)]
          [(g n1) (make-node g 'number 1)]
          [(g n2) (make-node g 'number 2)]
          [(g n3) (make-node g 'number 3)]
          [(g n4) (make-node g 'number 4)]
          [(g t1) (make-tag g '(a-tag) n1)]  ; TODO Just 'a-tag, not '(a-tag)
          [(g t2) (make-tag g '(a-tag) n2)]
          [(g t4) (make-tag g '(a-tag) n4)]
          [_ (check-equal? (list->set (filter/g g non-tag? (all-nodes g)))
                           (set n1 n2 n3 n4))]
          [_ (check-equal? (list->set (filter/g g tag? (all-nodes g)))
                           (set t1 t2 t4))])
      (void)))
  
  
  )
