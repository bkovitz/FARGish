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



; Experiment in building up the scout-state function one single-argument
; closure at a time.

(struct scout-state* (dragnets candidatess follow-upss total-promisingness
                      actions) #:prefab)

(define (desideratum->scout-state->t+1 desideratum)
  (let* ([items (desideratum->items desideratum)]
         [ls/dragnet->t+1 (map item->dragnet->t+1 items)]
         [followup-definitions (desideratum->followup-definitions desideratum)]
         [ls/follow-ups->new-candidatess->follow-ups
           (map followup-definition->follow-ups->new-candidatess->follow-ups
                followup-definitions)]
         [ls/follow-up->promisingness
           (map followup-definition->follow-up->promisingness
                followup-definitions)]
         [ls/promisingness->actions
           (map followup-definition->promisingness->actions
                followup-definitions)]
         [ls/follow-up->actions
           (map compose1 ls/promisingness->actions
                         ls/follow-up->promisingness)]
         [follow-upss->total-promisingness
           (followup-definitions->follow-upss->total-promisingness
             followup-definitions)])
    (λ (g scout)
      (let* (;;; get data for time t
             [scout-state (g:get-node-attr g scout 'state)]
             [old-candidatess (scout-state*-candidatess scout-state)]
             [old-dragnets (scout-state*-dragnets scout-state)]
             [old-followupss (scout-state*-follow-upss scout-state)]
             ;;; set up closures
             [ls/dragnet->new-candidates
               (map candidates->dragnet->new-candidates old-candidatess)]
             [ls/new-candidates->candidates
               (map candidates->new-candidates->candidates old-candidatess)]
             [new-candidatess->follow-upss
               (map apply ls/follow-ups->new-candidatess->follow-ups
                          old-followupss)]
             [ls/follow-ups->actions
               (append-map (curry map ls/follow-up->actions)
                           old-followupss)]
             ;;; make data for time t+1
             [dragnets (map apply ls/dragnet->t+1 old-dragnets)]
             [new-candidatess (map apply ls/dragnet->new-candidates dragnets)]
             [candidatess (map apply ls/new-candidates->candidates
                                     new-candidatess)]
             [follow-upss (new-candidatess->follow-upss new-candidatess)]
             [actions (map apply ls/follow-ups->actions follow-upss)]
             [total-promisingness
               (follow-upss->total-promisingness follow-upss)])
        (scout-state* dragnets candidatess follow-upss total-promisingness
                      actions)))))

; Still to be written

desideratum->items
item->dragnet->t+1
desideratum->followup-definitions
followup-definition->follow-ups->new-candidatess->follow-ups
followup-definition->follow-up->promisingness
followup-definition->promisingness->actions
followup-definitions->follow-upss->total-promisingness
candidates->dragnet->new-candidates
candidates->new-candidates->candidates


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
