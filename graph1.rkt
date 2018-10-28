; graph1.rkt -- Data structure for port graphs

#lang debug at-exp racket

(require "wheel.rkt" "sigs.rkt" "id-set.rkt")
(require (for-syntax racket/syntax) racket/syntax)
(require racket/hash)
(require rackunit racket/pretty describe debug/repl racket/enter)

;(require rackunit racket/generic racket/struct "id-set.rkt"
;         racket/dict racket/pretty describe mischief/memoize) 
;(require racket/serialize)

; TEMPORARY version: replace with graph-core^ from sigs.rkt.
(define-signature graph-core1^
  (empty-graph
   graph?

   make-node  ; g attrs -> g nodeid
   ;remove-node
   ;add-node
   ;add-edge
   
   has-node?
   ;has-edge?
   ;all-nodes
   ;all-edges

   ;port->neighboring-ports
   ;port->neighbors
   ;port->neighbor
   ;port->port-label->nodes 
   ;port->incident-edges

   ;node->neighbors
   ;node->ports
   ;node->incident-edges

   ;define/g
   ;gdo
   ))

(define-signature graph-name^
  (ensure-node-name)) ; g attrs -> attrs name

(define-unit simple-graph-name@
  (import)
  (export graph-name^)

  (define (ensure-node-name g attrs)
    (define (set-name-and-return name)
      (values (hash-set attrs 'name name) name))
    (cond
      [(hash-ref attrs 'name #f)
       => (λ (name) (values attrs name))]
      [(hash-ref attrs 'value #f)
       => set-name-and-return]
      [(hash-ref attrs 'class #f)
       => set-name-and-return]
      [else (set-name-and-return 'UNKNOWN)])))

(define-unit graph-core@
  (import graph-name^)
  (export graph-core1^)

  (struct graph (ht-node->attrs
                 ht-port->neighboring-ports
                 edges
                 id-set
                 stacks  ; dict of temp vars for do-graph-edits
                 vars    ; hash-table of vars: name -> value
                 spec) #:prefab)

  (define empty-spec #hash())
  (define empty-graph
    (graph #hash() #hash() #hash() empty-id-set #hash() #hash() empty-spec))

  ; Returns two values: g nodeid
  (define (make-node g attrs)
    (let*-values ([(attrs name) (ensure-node-name g attrs)]
                  [(id-set id) (gen-id (graph-id-set g) name)]
                  [(attrs) (hash-set attrs 'id id)]
                  [(g) (let ([ht (graph-ht-node->attrs g)])
                         (struct-copy graph g
                           [ht-node->attrs (hash-set ht id attrs)]
                           [id-set id-set]))])
      (values g id)))
  
  (define (has-node? g id)
    (hash-has-key? (graph-ht-node->attrs g) id))
  )

(module+ test
  (define-compound-unit/infer g@
    (import)
    (export graph-name^ graph-core1^)
    (link simple-graph-name@ graph-core@))

  (define-values/invoke-unit g@ (import) (export graph-core1^))

  (test-case "make-node"
    (let*-values ([(g) empty-graph]
                  [(_) (check-false (has-node? g 5))]
                  [(g id1) (make-node g #hash((value . 5)))]
                  [(_) (check-equal? id1 5)]
                  [(_) (check-true (has-node? g 5))])
      (void)))
  )
