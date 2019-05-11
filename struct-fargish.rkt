; struct-fargish -- struct definitions in Typed Racket for FARGish elements
;
; The macros that convert the FARGish syntax tree into Racket code generate
; code that builds these structs.

#lang debug typed/racket
(require typed/debug/report)

(require errortrace)
(require (for-syntax syntax/parse))
(require "typed-wheel.rkt" typed/racket/flonum)

(provide ;(struct-out Nodeclass)
         ;(struct-out Arg)
         ;(struct-out Nodeclass-Body-Elem)
         ;(struct-out Codelet)
         ;(struct-out NamedNode)
         ;(struct-out NodeRef)
         ;(struct-out Expr)
         ;(struct-out ChainExpr)
         ;(struct-out ChainLink)
         )

; TODO Copy the structs over from fb2.rkt and add types to their members.
; TODO Copy their ancillary functions, etc.
