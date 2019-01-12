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

(define (acceptability/of-class class sk g result-so-far node)
  (cond
    [(m:node-is-a? g node class)
     (sk g (safe-* result-so-far 1.0) node)]
    [else 'reject]))

(define (acceptability/in-role port-label value sk g result-so-far node)
  (cond
    [(g:has-neighbor-from-port-label? g node port-label)
     (acceptability/value sk g (safe-* result-so-far 1.0) node)]
    [else 'reject]))


(define (acceptability/generic node-ok? sk g result-so-far node)
  (cond
    [(node-ok? g node)
     (sk g (safe-* result-so-far 1.0) node)]
    [else 'reject]))

(define (mk-acceptability/of-class class)
  (curry acceptability/generic (curryr m:node-is-a? class)))

(define (mk-acceptability/in-role port-label)
  (curry acceptability/generic (curryr g:has-neighbor-from-port-label?
                                       port-label)))

(define (mk-acceptability/value value)
  (curry acceptability/generic (curry 


(define (acceptability/generic node-measure sk g result-so-far node)
  (let ([measure (node-measure g node)])
    (cond
      [(eq? 'reject measure)
       'reject]
      [else (sk g (safe-* result-so-far measure) node)])))

(define (mk-all-or-nothing pred? . final-args)
  (λ (g node)
    (if (apply pred? g node final-args)
      1.0
      'reject)))

(define (mk-acceptability/of-class class)
  (curry acceptability/generic (mk-all-or-nothing m:node-is-a? class)))

(define (mk-acceptability/in-role port-label)
  (curry acceptability/generic (mk-all-or-nothing
                                 g:has-neighbor-from-port-label? port-label)))

(define (mk-acceptability/value target-value)
  (curry acceptability/generic
         (λ (g node)
           (let ([actual-value (g:value-of g node)])
             (cond
               [(void? actual-value)
                'reject]
               [else (some-sigmoid-thing target-value actual-value)])))))
