; xpbpb.rkt -- Throwaway code to figure out pasteboard within a pasteboard as
; a way to make composite active picts.

; Conclusion: it's not a good idea. The first click selects the containing
; pasteboard; the second click selects a pict on the inner pasteboard. Once
; you've grabbed one of the picts, it's too easy to slide it around on the
; inner pasteboard so that most of it is hidden because it goes beyond the
; rectangular extent of the inner pasteboard. The rectangularity of the
; inner pasteboard constantly matters.

#lang debug at-exp racket/gui
(require racket/class
         racket/snip
         racket/format
         pict
         pict/snip
         mrlib/snip-canvas)

(define frame
  (new frame%
       [label "Tests"]
       [width 500]
       [height 500]))

(define ec (new editor-canvas%
                [parent frame]))

(define pb (new pasteboard%))
(send ec set-editor pb)

(define (make-labeled-pict l p)
  (define pb (new pasteboard%))
  (define es (new editor-snip% [editor pb] [with-border? #f]))
  (define x (/ (max (pict-width l) (pict-width p)) 2.0))
  (define lx (- x (/ (pict-width l) 2.0)))
  (define ly 0)
  (define px (- x (/ (pict-width p) 2.0)))
  (define py (+ (pict-height l) 1.0))
  (send pb insert (new pict-snip% [pict l]) lx ly)
  (send pb insert (new pict-snip% [pict p]) px py)
  es)

(send pb insert (make-labeled-pict
                  (text "this")
                  (disk 10.0 #:draw-border? #f #:color "Aquamarine")))

(send frame show #t)
