; support-typed -- Support network
;
; Provides functions from support-core.rkt as well as convenience functions.

; TODO Rename this to support.rkt and remove/rename the untyped version when
; this is done.

#lang typed/racket

(require "typed-wheel.rkt" "types.rkt" "model.rkt" "support-core.rkt")

(provide (all-from-out "support-core.rkt")
         make-permanent)


;STUB
(: make-permanent : Graph (U Node Void) * -> Graph)
(define (make-permanent g . node)
  g)
