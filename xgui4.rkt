#lang racket/gui
(require racket/class
         racket/snip
         racket/format
         pict
         pict/snip)

(define frame
  (new frame%
       [label "Example"]
       [width 300]
       [height 300]))

(define msg
  (new message%
       [parent frame]
       [label "No events so far..."]))

(define ec (new editor-canvas%
                [parent frame]))

(define pb (new pasteboard%))
(send ec set-editor pb)

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    (super-new)))

(define hover-pict-snip%
  (class pict-snip%
    (define/override (on-event dc x y editorx editory event)
      (send msg set-label "Hover!"))
    (super-new)))

(define ps (new hover-pict-snip% [pict (disk 20 #:color "teal")]))
(send ps set-flags '(handles-all-mouse-events))

(send pb insert ps)

(send frame show #t)