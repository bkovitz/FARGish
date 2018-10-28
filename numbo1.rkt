; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "fargish.rkt" (prefix-in g: "graph.rkt")
         (prefix-in g: "make-graph1.rkt")
         (only-in "graph.rkt" pr-graph pr-group pr-node))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)

;; ======================================================================
;;
;; The spec
;;

(define spec
  (farg-model-spec
    (nodeclass numbo-ws
      (is-a 'ctx))
    (nodeclass equation
      (is-a 'ctx))
    (nodeclass number)
    (nodeclass target
      (is-a 'number)
      (links-into 'ctx (by-ports 'target 'result) as-member))
    (nodeclass brick
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'result) as-member))
    (nodeclass operator)
    (nodeclass +
      (is-a 'operator))
    (nodeclass -
      (is-a 'operator))
    (nodeclass *
      (is-a 'operator))
    (nodeclass /
      (is-a 'operator))
    (tagclass (needs source)
      (is-a 'problem-tag)
      (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
        (condition (no-neighbor-at-port?/g 'source node))))
    ))

;; ======================================================================
;;
;; Making a workspace
;;

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g 'numbo-ws)]
                [(g) (add-nodes/in g ws 'brick bricks)]
                [(g) (add-node/in g ws 'target target)])
    (values g ws)))

(define (make-numbo-g . args)
  (first-value (apply make-numbo-ws (make-empty-graph spec) args)))

#;(module+ test
  (test-case "numbo-ws"
    (let*-values ([(g) (make-empty-graph spec)]
                  ;[(_) (pretty-print (get-spec g))]
                  [(g ws) (make-numbo-ws g '(4 5 6) 15)])
      (pr-graph g)
      #f
      ;TODO
      )))

;; ======================================================================
;;
;; Searching the numbo-ws

(define (done? g)
  (define (has-source? node)
    (cond
      [(node-is-a? g 'number node)
       (define sources (port->neighbors g `(,node source)))
       (if (empty? sources)
         #f
         (for/and ([source sources])
           (has-source? source)))]
      [(node-is-a? g 'operator node)
       (define operands (port->neighbors g `(,node operands)))
       (if (empty? operands)
         #f
         (for/and ([operand operands])
           (has-source? operand)))]
      [(node-is-a? g 'numbo-ws node)
       #t]))
  ;TODO? throw error if no match?

  (has-source? (port->neighbor g '(numbo-ws target))))

(module+ test
  (let ([g (make-numbo-g '(4 5 6) 15)])
    (check-false (done? g))
    


    (let ([g (g:do-graph-edits g '((:begin (:node number 9) (:node operator + +)
                                   (:edge (brick4 result) (+ operands))
                                   (:edge (brick5 result) (+ operands))
                                   (:edge (+ result) (number9 source)))))])
      (check-false (done? g))
      (let ([g (g:do-graph-edits g '((:let ([+ (:node operator + +)])
                                     (:edge (number9 result) (+ operands))
                                     (:edge (brick6 result) (+ operands))
                                     (:edge (+ result) (target15 source)))))])
        (check-true (done? g)) ))))
