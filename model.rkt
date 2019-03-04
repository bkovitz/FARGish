; model.rkt -- Read/write interface to a FARG model
;
; Essentially an interface to graph.rkt that knows about the FARGish spec.
; Provides all the same functions as graph.rkt, with a few overridden, plus
; some more.

#lang debug at-exp typed/racket

(require debug/repl errortrace)
(require "typed-wheel.rkt")
(require (prefix-in g: (only-in "graph.rkt"
                                [make-node g:make-node]
                                [add-node g:add-node]))
         (except-in "graph.rkt" make-node add-node))
(module+ test (require typed/rackunit))

(provide (all-from-out "graph.rkt"))
