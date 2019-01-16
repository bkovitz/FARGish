#lang debug at-exp racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require racket/hash)
(require "wheel.rkt")
(require rackunit)

(require (prefix-in m: "model1.rkt")
         "shorthand.rkt"
         (prefix-in e: "equation.rkt")
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass portclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))


; Don't bother searching for tags on equation. Just search by crude criteria,
; guided only by slipnet activations.
;
; NEXT Get initial archetypes by searching the desideratum for values.
; Might need to get values of nodes referenced, maybe examine their tags.

'(node (of-class equation)
       (splice-into ws
         (bind-to 15 (in-role result))
         (bind-to 4 (in-role operand))
         (bind-to 5 (in-role operand))))

; First scout searches slipnet for equations. sk: run 2nd scout

; Second scout runs the splicing process. Starts scouts for the bind-tos,
; then starts scouts to bind/build from the equation nodes. sk: make the
; built nodes permanent.

; Bind/build scouts build if they can't find a mate.

; The 'bind nodes start more scouts: for neighbor-bindings.


; How do we break this into small, testable pieces?
;   mk-make-initial-archetypes
;   Test against list of actions without running them.

; The splicing scout needs to make its own desideratum:

'(all-or-nothing
   (bind (from '4+5=9) (to 'ws)
     (bind-to 15' (in-role result))  ; how do we bend on this?
     (bind-to 4' (in-role operand))
     (bind-to 5' (in-role operand))
     (bind-or-build-from 9)
     (bind-or-build-from +)
     (bind-or-build-from 4)
     (bind-or-build-from 5)))

; Promisingness might be able to guide releasing one or more desiderata
; from 'all-or-nothing. The support network could do that: if the rest of
; the system favors some of the bindings, we could accept an imperfect
; splice. A simple solution: if at least one bind-to is fulfilled and
; all the preimage nodes are bound to a mate, then declare victory.



(define (desideratum->scout->actions desideratum)
  (let* ([items (desideratum->items desideratum)]
         [ls/dragnet->t+1 (map item->dragnet->t+1 items)]
         [ls/dragnet->candidates (map item->dragnet->candidates items)]
         [. . . more . . .])
    (λ (g scout)
      (let* ([dragnets (map apply ls/dragnet->t+1 (dragnets-of g scout))]
             [candidatess (map apply ls/dragnet->candidates dragnets)]
             [. . . more . . .])
        return a list of actions))))

; Update the dragnets of search-items that lack promising candidates.
; Update the candidate sets of the search-items whose dragnets changed.
; Start follow-ups that depend on search-items with new candidates.
; Support promising follow-ups; try to save unpromising ones;
; lock in completed follow-ups; cancel failed follow-ups.
; Update the scout's total promisingness.

; Illustrates that g is a different sort of argument.
(define (scout->actions g scout)
  (let* ([g->actions (scout->g->actions g scout)])
    (g->actions g)))
