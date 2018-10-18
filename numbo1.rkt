; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require "wheel.rkt" "xsusp3.rkt" (prefix g: "graph.rkt") "make-graph.rkt")
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)

;; Making a workspace

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g '((class . numbo-ws)))]
                [(g) (for/fold ([g g])
                               ([brick bricks])
                       (let*-values ([(g brickid) (make-node g brick)]
                                     [(g) (add-edge g `((,ws bricks)
                                                        (,brickid source)))]
                                     [(g) (add-edge g `((,ws members)
                                                        (,brickid
                                                          member-of)))])
                         g))]
                [(g targetid) (make-node g target)]
                [(g) (add-edge g `((,ws target) (,targetid result)))]
                [(g) (add-edge g `((,ws members) (,targetid member-of)))])
    g))

(struct nodeclass (name ))

(struct by-ports (from-port to-port) #:prefab)

(define as-member (by-ports 'members 'member-of))

(define brick (nodeclass (brick n)
                (is-a 'number)
                (links-into 'ctx (by-ports 'bricks 'source) as-member)

(define numbo
  (farg-model
    (nodeclass ws
      (is-a 'ctx))
    (nodeclass (number n))
    (nodeclass (brick n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (nodeclass (target n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'target 'result) as-member))
    (nodeclasses (+ - * /)
      (is-a 'operator))
    (tagclass (needs need-type)
      (enum need-type 'source 'result 'greater-result)
      (is-a 'problem-tag)
      (applies-to (node (of-class 'number) (by-ports 'tagged 'tags))
        (condition (case need-type
                     [(source) (no-neighbor-at-port? 'source node)]
                     [(result) (no-neighbor-at-port? 'result node)]
                     [(greater-result)
                      (and? (has-tag?/g '(needs result) node)
                            (any-of?/g (has-tag?/g '(needs source))
                              (walk node '(tag >) '(mate greater))))])))
      (mate 'solution `(fills-port ,(value-of g node) ,need-type)))
    (tagclass >
      (applies-to (lesser (of-class 'number) (by-ports 'lesser 'tags))
                  (greater (of-class 'number) (by-ports 'greater 'tags))
        (condition (value-pred? > lesser greater))))
    (tagclass =
      (applies-to (node1 (of-class 'number) (by-ports 'tagged 'tags))
                  (node2 (of-class 'number) (by-ports 'tagged 'tags))
        (condition (value-pred? = node1 node2))))
    (tagclass doubled-operands
      (applies-to (node1 (of-class 'number) (by-ports 'tagged 'tags))
                  (node2 (of-class 'number) (by-ports 'tagged 'tags))
        (condition (and? (value-pred? = node1 node2)
                         (no-neighbor-at-port? 'result node1)
                         (no-neighbor-at-port? 'result node2)))))
    ))

;; Another way to do (needs greater-result)

(tagclass (needs 'greater-result) ; specialization (quoted) overrides variable
  (is-a 'problem-tag)
  (applies-to (node (of-class 'number)) ; default by-ports
    (condition (and?/g (has-tag?/g '(needs result) node)
                       (any-has-tag?/g '(needs source)
                         (mates-via/g node '> 'greater))))))

;; This defines two functions, has-tag? and has-tag?/g. The second one is:
;;  (λ (g . args) (apply has-tag? g args))
(define/g (has-tag? g tagspec node)
  ...)

(define empty-set (set))
(define empty-hash (hash))

(struct is-a (ancestors) #:prefab #:constructor-name make-is-a)
; ancestors: (Setof Symbol)

(define (is-a . args)
  (make-is-a (list->set args)))

(struct by-ports (from-port to-port) #:prefab)

(struct links-into (ctx-class by-portss) #:prefab
                   #:constructor-name make-links-into)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define (links-into ctx-class . by-portss)
  (make-links-into ctx-class by-portss))

(struct nodeclass (name ancestors links-intos) #:prefab
                  #:constructor-name make-nodeclass)

(define (mk-nodeclass name . args)
  (for/fold ([ancestors empty-set]
             [links-intos empty-hash]
             #:result (make-nodeclass name ancestors links-intos))
            ([arg args])
    (match arg
      [(is-a ancestors-)
       (values (set-union ancestors ancestors-) links-intos)]
      [(links-into ctx by-portss)
       (values ancestors (hash-set links-intos ctx))])))

(define (add-brick-to g ctx n)
  (let*-values ([(g brickid) (

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g '((class . numbo-ws)))]
                [(g) (for/fold ([g g])
                               ([brick bricks])
                       (add-brick g ws brick))]
                [(g) (add-target g ws target)])
    g))

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g 'ws)]
                [(g) (add-nodes-into g ws 'brick bricks)]
                [(g) (add-node-into g ws 'target target)])
    g))

(define (make-numbo-ws g bricks target)
  (~>graph [g g]
    #:let ([ws (make-node 'ws)])
    (for ([brick bricks])
      (add-node-into ws 'brick brick))
    (add-node-into ws 'target target)))

#;(module+ test
  (test-case "numbo-ws"
    (let ([g (make-numbo-ws (make-graph) '(4 5 6) 15)])
      #f
      ;TODO
      )))

