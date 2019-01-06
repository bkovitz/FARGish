; scout.rkt -- Scout nodes: agent nodes that search the FARG graph for something

#lang debug at-exp racket

(require "wheel.rkt"
         "observe.rkt"
         "model1.rkt"
         "spreading-activation.rkt"
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))

(require racket/hash)
(require racket/generic)
(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

(provide make-scout-state
         step
         scout-sa-params
         scout-state-activations
         scout-state-results)

;; ======================================================================
;;
;; scout-state
;;

(define scout-sa-params (make-parameter default-spreading-activation-params))

(struct scout-state* (node->neighbors select-results activations results)
        #:prefab)
; node->neighbors : g node -> (Set node)
; select-results : g activations -> (List node)  ; leading results, if any

(define (make-scout-state node->neighbors select-results initial-activations)
  (scout-state* node->neighbors select-results initial-activations '()))

(define scout-state-activations scout-state*-activations)
(define scout-state-results scout-state*-results)

; Spreads activation num-steps steps and updates results; returns new
; scout-state*.
(define (step g scout-state [num-steps 1])
  (define node->neighbors (scout-state*-node->neighbors scout-state))
  (define select-results (scout-state*-select-results scout-state))
  
  (define (one-step activations)
    (spread-activation (scout-sa-params)
                       activations
                       (λ (node) (node->neighbors g node))
                       (const 1.0)))

  (define activations (for/fold ([as (scout-state-activations scout-state)])
                                ([i num-steps])
                        (one-step as)))
  (define results (select-results g activations))
  (struct-copy scout-state* scout-state
               [activations activations]
               [results results]))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (require "shorthand.rkt"
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass by-ports as-member))

  (define spec
    (farg-model-spec
      (nodeclass (number n)
        (value n)
        (name n))
      (nodeclass (letter a)
        (value a)
        (name a))))

  (test-case "Simple scout test"
    (parameterize ([scout-sa-params (set-weight-noisef (scout-sa-params)
                                                       identity)])
      (define g (make-graph spec '(:let ([1 (number 1)]
                                         [2 (number 2)]
                                         [3 (number 3)]
                                         [a (letter a)])
                                     (:edge 1 out 2 in)
                                     (:edge 1 out a in)
                                     (:edge 2 out 3 in))))
      (define (node->neighbors g node)
        (for/set ([neighbor (g:port->neighbors g `(,node out))]
                  #:when (node-is-a? g neighbor 'number))
          neighbor))

      (define activation-threshold 0.1)

      (define (select-results g activations)
        (define activation-of (hash->numeric-f activations))
        (define (good-enough? node) (>= (activation-of node)
                                        activation-threshold))
        (define acceptable-nodes (filter good-enough? (hash-keys activations)))
        (sort acceptable-nodes >))

      (define scout-state (make-scout-state node->neighbors
                                            select-results
                                            (hash 1 1.0)))

      (check-equal? (scout-state-results scout-state)
                    '())
      (check-equal? (scout-state-results (step g scout-state 4))
                    '(2 1))
      (check-equal? (scout-state-results (step g scout-state 6))
                    '(3 2 1))))


;  (test-case "Realistic scout test"
;    ; Set up graph: 

)
