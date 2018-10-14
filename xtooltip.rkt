; xtooltip.rkt -- Throwaway code: experimenting with making a tooltip

#lang debug at-exp racket/gui

(require (prefix-in pict: pict) pict/snip mrlib/snip-canvas)
;(require drracket/private/tooltip)

(define fr (new frame% [label "xtooltip"] [width 200] [height 100]))

;CONFUSION: This is needed only because pane% doesn't support client->screen.
;Is the reason why it doesn't also a reason why this function shouldn't exist?
(define (window-parent-of window)
  (let ([parent (send window get-parent)])
    (cond
      [(not parent)
       #f]
      [(is-a? parent window<%>)
       parent]
      [else (window-parent-of parent)])))

;CONFUSION: Is the documentation on client->screen or get-current-mouse-state
;wrong?
(define-values (screen-x-offset screen-y-offset)
  (let-values ([(xo yo) (get-display-left-top-inset)])
    (values (- xo) (- yo))))
(define (window-top-left-in-screen-coordinates window)
  (let ([parent (window-parent-of window)])
    (if parent
      (let-values ([(wx wy) (send parent client->screen (send window get-x)
                                                        (send window get-y))])
        (values (+ wx screen-x-offset) (+ wy screen-y-offset)))
      (values (send window get-x) (send window get-y)))))

(define (in-window? window point)  ; <--- CODE SMELL: reinventing the wheel?
  (define-values (wx wy) (window-top-left-in-screen-coordinates window))
  (define-values (ww wh) (send window get-size))
  (define-values (px py) (values (send point get-x) (send point get-y)))
  (and (<= wx px (+ wx ww))
       (<= wy py (+ wy wh))))

(define (text->tooltip-pict text)
  (let* ([text (if (pair? text) (map ~a text) (string-split (~a text) "\n"))]
         [text-image (for/fold ([text-image (pict:blank)])
                               ([line text])
                       (pict:vl-append text-image (pict:text line)))]
         [text-image (pict:inset text-image 4 2)]
         [background (pict:filled-rectangle
                       (ceiling (pict:pict-width text-image))
                       (ceiling (pict:pict-height text-image))
                       #:color "LemonChiffon"
                       #:draw-border? #t)])
    (pict:cc-superimpose background text-image)))

(define pict-canvas%  ; <--- CODE SMELL: reinventing the wheel (pict.rkt)
  (class canvas%
    (init-field pict
                [style '()])
    (inherit get-dc)
    (define/override (on-paint)
      (pict:draw-pict pict (get-dc) 0 0))
    (super-new [min-width (exact-ceiling (pict:pict-width pict))]
               [min-height (exact-ceiling (pict:pict-height pict))]
               [stretchable-width #f]
               [stretchable-height #f]
               [style (cons 'transparent style)])))

(define tooltip-window%
  (class frame%
    (init-field text
                point ; will place window above this point
                [pict (text->tooltip-pict text)])
    (define width (exact-ceiling (pict:pict-width pict)))
    (define height (exact-ceiling (pict:pict-height pict)))
    (super-new [style '(no-resize-border no-caption float)]
               [label ""]
               [width width]
               [height height]
               [stretchable-width #f]
               [stretchable-height #f]
               [x (exact-ceiling (- (send point get-x) (/ width 2) 3))]
               [y (exact-ceiling (- (send point get-y) height 8))])
    (define canvas (new pict-canvas% [pict pict] [parent this]))
    (send this show #t)))

(define TOOLTIP-HOVER-DELAY 600)
  ;When mouse cursor sits motionless over relevant window for this long,
  ;tooltip appears.

(define tooltip-mixin
  (mixin (window<%>) (window<%>)
    (init-field [tooltip (void)]
                [tooltip-window #f])
    (super-new)

    (define (maybe-open-tooltip-window)
      (define-values (point buttons) (get-current-mouse-state))
      (when (and (null? buttons) (in-window? this point))
        (set! tooltip-window (new tooltip-window% [text tooltip]
                                                  [point point]))))

    (define timer
      (new timer% [notify-callback maybe-open-tooltip-window]))

    (define/public (close-tooltip-window)
      (send tooltip-window show #f) ;<--- MEMORY LEAK: Should close, not hide
      (set! tooltip-window #f))

    (define/override (on-subwindow-event receiver e)
      (if (and (not (void? tooltip))
               (eq? this receiver)
               (eq? 'motion (send e get-event-type)))
               ;STRANGE: We never get 'enter or 'leave events
        (begin
          (if tooltip-window
            ; If tooltip is showing, mouse motion closes it
            (close-tooltip-window)
            ; Mouse motion followed by a pause opens it
            (send timer start TOOLTIP-HOVER-DELAY #t))
          #t)  ; UNSURE: What is on-subwindow-event supposed to return here?
        #f))))
      ;BUG: Often no 'motion event comes when the mouse leaves this window,
      ;so the tooltip stays up.

(define m (new (tooltip-mixin message%)
               [parent fr] [label "Label"] [tooltip 22.6]))
(define b (new (tooltip-mixin button%)
               [parent fr] [label "Button"] [tooltip "Press this"]))

(define hp (new horizontal-pane% [parent fr] [alignment '(left top)]))

(define tpc% (tooltip-mixin pict-canvas%))

(define (disk d)
  (pict:cc-superimpose
    (pict:ghost (pict:disk 50))
    (pict:disk d #:color "aquamarine" #:draw-border? #f)))

(define (make-dot parent label activation)
  (define vp (new vertical-pane% [parent parent]
                                 [stretchable-width #f]
                                 [stretchable-height #f]))
  (define l (new message% [parent vp] [label label]))
  (define d (new tpc% [parent vp]
                      [pict (disk (* 8.0 activation))]
                      [tooltip activation]))
  vp)

;(define tpc1 (new tpc% [parent hp] [pict (disk 30)] [tooltip 22.6]))
;(define tpc2 (new tpc% [parent hp] [pict (disk 3)] [tooltip 2.6]))
(define d1 (make-dot hp "archetype4" 4.1))
(define d2 (make-dot hp "some-sa-node" 2.26))
(define d3 (make-dot hp "this-dot" 0.4))

(define inert-pasteboard%  ; <--- CODE SMELL: reinventing wheel?
  (class pasteboard%
    (define/augment (can-select? _)
      #f)
    (super-new)))

(define ROW-HEIGHT 100)
(define LABEL-OFFSET 5)
(define DOT-CENTER-OFFSET 65)
(define DIAMETER-RATIO 8.0)

;(define sa-canvas%
;  (class editor-canvas%
;    (init-field [max-width 1000]
;                [pb (new inert-pasteboard%)])
;    (define current-row 0)
;    (define current-x 0)
;    (define/public (insert-dot label activation-level)
;      (send pb insert (new pict-snip%
;    (super-new [editor pb])))

(send fr show #t)

;(define tf (new tooltip-frame% [frame-to-track m]))
;(send tf set-tooltip (list "tooltip text"))
;(let*-values ([(x y w h) (values (send m get-x)
;                                 (send m get-y)
;                                 (send m get-width)
;                                 (send m get-height))]
;              [(x y) (send m client->screen x y)])
;  #R x #R y
;  (send tf show-over x y w h))
;#;(send tf show #t)

; QUESTIONS
;
;  How do you close a frame in racket/gui?
;
;  What are "screen coordinates"? Are they different for client->screen
;  and get-current-mouse-state?
;
;  Isn't snip-canvas% there so you can have an active "snip" GUI object
;  without an editor? But doesn't that undermine the point of snips--objects
;  within an editor?
;
;  Probable bug: overlapping windows could have both tooltips showing
;  simultaneously.
