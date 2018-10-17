; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash profile describe
         "wheel.rkt" "xsusp3.rkt" "graph.rkt" "make-graph.rkt")

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

#;(module+ test
  (test-case "numbo-ws"
    (let ([g (make-numbo-ws (make-graph) '(4 5 6) 15)])
      #f
      ;TODO
      )))

