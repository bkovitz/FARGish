; Ideas for syntax to put definitions for dragnets, search-items, etc. into
; spec.


(define-state (dragnet d-impl)
  t0 d-impl
  t+1 (if candidates-not-good-enough?
        (advance d-impl)
        d-impl))

(define-state (spreading-activation root)
  t0 (apply hash (append-map archetype+salience start-from))
  t+1 (spread-activation t))

(define-agent (find ([node stuff ...] ...) follow-ups ...)
  
;Main ideas:
;  Define stateful elements with t0 and t+1.
;  Refer to other state variables.
;  Return a list of actions?
;  Macro-expand these into functions that generic code can call one at a time.

; "eval" the 'find, get a list of actions?
; eval/t0 eval/t eval/t+1

'(find ([eqn (dragnet (spreading-activation 'slipnet)
                      (such-that (of-class 'equation))
                      (start-from 4 5 15))
             (promisingness ???)])
   (for-each-unique [eqn]
      (splice-into 'ws eqn))
   t promisingness (max (promisingness eqn)))

'(let* ([search-items ([eqn (candidates ???)])]
        [follow-ups (splice-into 'ws eqn)]
        [(promisingness t) (min-promisingness search-items)])
   follow-ups->actions)

'(define (search-items name candidate promisingness t)
   (let* ([follow-ups (
   (apply max-promisingness (min-follow-up-for-each-definition name candidate)))

; Could we do all this with ports and edges?
; name  is a port
; candidate  is a neighbor of name
; follow-up-definition  is a port
; follow-up  is a neighbor of follow-up-definition
; promisingness   is a function

; A path:
'((search-item eqn) * promisingness)
'((search-item eqn) * promisingness min)
'((search-item 5′) * follow-up)   ; an internal path?
; No good. The scout needs its own estimate of promisingness for each
; search-item. A candidate node's intrinsic promisingness, if any, probably
; won't be relevant.

; Could a node's attrs be free variables as in Racket's classes?
(nodeclass scout
  (attr search-items)
  (attr followup-definitions)
  (define/t+1 search-items
    (



(define-state (candidate-set name . attrs)
  [t0  (attrs.dragnet t0)
       (attrs.promisingness 0.2)
       (attrs.candidates empty-set)]
  [t+1 (when (<= attrs.promisingness 0.8)
         (attrs.dragnet t+1)
         (attrs.candidates (merge-new attrs.dragnet attrs.such-that)))
       (attrs.promisingness decay)])
; Each candidate should have its own promisingness.
