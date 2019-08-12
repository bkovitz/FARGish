#lang racket

;; labeled disks with pin/unpin behavior

(require racket/gui
         racket/snip
         racket/class
         mrlib/snip-canvas
         pict
         pict/snip)

(define frame
  (new frame%
       [label "Tests"]
       [width 500]
       [height 500]))

(define ec (new editor-canvas% [parent frame]))

(define pin-pasteboard%
  (class pasteboard%
    (super-new)
    (define/augment (can-select? snip on?)
      (not (send snip get-pinned?)))))

(define pb (new pin-pasteboard%))
(send ec set-editor pb)

(define (labeled-disk name color diameter)
  (vc-append (text name) (disk diameter #:color color)))

(define labeled-disk%
  (class pict-snip%
    (init-field name [color "teal"] [diameter 10] [pinned? #f])
    (define/public (get-pinned?) pinned?)
    (define/public (set-pinned? b) (set! pinned? b))
    (define/public (pin) (set-pinned? #t))
    (define/public (unpin) (set-pinned? #f))
    (super-new [pict (labeled-disk name color diameter)])))

(define p1
  (new labeled-disk% [name "test1"] [diameter 20]))
(define p2
  (new labeled-disk% [name "test2"] [diameter 5]))

(send pb insert p1 10 10)
(send pb insert p2 60 10)

(send frame show #t)