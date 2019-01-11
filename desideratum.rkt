; desideratum.rkt -- Desiderata: things to search for and build

(require "wheel.rkt" "observe.rkt")

(require racket/hash)
(require racket/generic)
(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

; Put stringency on any desideratum?

'(in-role result)
'(in-role result (value 5))
'(in-role result (value 5 (stringency 0.7)))
'(has-member (in-role result (value 5)))
'(and (has-members (in-role ...) (in-role ...)))

'(and (has-tag (has-member (in-role ...)) (has-member (in-role ...))))
; A tag should simply provide a shortcut for determining info about the
; members.


'(looks-like ((2 ?operator 3) makes 5) (in-space equation))
; So just map this thing to a space and a number, and map the other node the
; same way, and see how close they are. Maybe specify a space, and let the
; scout alter that space if the search doesn't go well.
;
; (20 + 30 makes 50) should have a good match with this. So, we need a
; logarithmic representation anchored at some appropriate spots.

; NEXT Figure out how to specify a "space".


; Chain desiderata closures together via continuations and other args.
(define (acceptability g sk so-far bdx node)

; The search should return "pre-bindings".
