; Throwaway code to see how to display spreading activation graphically

#lang debug at-exp racket/gui

(require rackjure/threading)
(require pict pict/color data/collection data/pvector)
(require describe)

(define is '#hash(
  (archetype-fills-port-15-source . 1.0)
  (archetype-fills-port-4-result . 1.0)
  (archetype-fills-port-5-result . 1.0)
  (archetype-fills-port-6-result . 1.0)
  (archetype-fills-port-greater-result-4 . 1.0)
  (archetype-fills-port-greater-result-5 . 1.0)
  (archetype-fills-port-greater-result-6 . 1.0)
  (archetype15 . 1.0)
  (archetype4 . 1.0)
  (archetype5 . 1.0)
  (archetype6 . 1.0)))

(define (strip-prefix s prefix)
  (if (string-prefix? s prefix)
    (substring s (string-length prefix))
    #f))

(define (archetype-symbol->s sym)
  (define s (symbol->string sym))
  (or (strip-prefix s "archetype-")
      (strip-prefix s "archetype")
      s))

(define (activations->archetype-names activations)
  (map (λ (pair) (archetype-symbol->s (car pair))) (hash->list activations)))

(struct cell (archetype name text disk pict) #:transparent)

(define max-width 147)
(define max-height 20)
(define layout-width 900.0)
(define awidth (+ max-width 10.0))
(define aheight (+ max-height 20.0))
(define cell-ghost (ghost (rectangle awidth aheight)))

(define (make-cell archetype)
  (define name (archetype-symbol->s archetype))
  (define t (text name))
  (define d (disk 15 #:draw-border? #f #:color "Aquamarine"))
  (define p (inset
              (cb-superimpose (ct-superimpose cell-ghost t) d)
              0.0 10.0))
  (cell archetype name t d p))

(define (make-layout0 activations)
  (define ts (map text (activations->archetype-names activations)))
  (define max-width (apply max (map pict-width ts)))
  (define max-height (apply max (map pict-height ts)))
  (define cells (map (λ (t)
                       (inset
                         (cb-superimpose
                           (ct-superimpose cell-ghost t)
                           (disk 15 #:draw-border? #f #:color "Aquamarine"))
                         0.0 10.0))
                     ts))
  (for/fold ([rows (pvector)] #:result (apply vl-append rows))
            ([cell cells])
    (cond
      [(empty? rows)
       (conj rows cell)]
      [(> (+ (pict-width cell) (pict-width (last rows)))
          layout-width)
       (conj rows cell)]
      [else (set-nth rows
                     (sub1 (length rows))
                     (hb-append (last rows) cell))])))

(struct layout (cells pict) #:transparent)

(define (lay-out-rows items [max-width layout-width])
  (for/fold ([pict (blank)] [row (blank)] #:result (vl-append pict row))
            ([item items])
    (define item-width (pict-width item))
    (define row-width (pict-width row))
    (if (> (+ row-width item-width) max-width)
      (values (vl-append pict row) item)
      (values pict (hb-append row item)))))

(define (make-layout activations)
  (define cells (for/hasheq ([a (hash-keys activations)])
                  (values a (make-cell a))))
  (define sorted-cells (sort (hash-values cells) string<? #:key cell-name))
  (layout cells (lay-out-rows (map cell-pict sorted-cells))))


;;

(define (mkdisk color) (disk 15 #:draw-border? #f #:color color))
(define d1 (mkdisk "Aquamarine"))
(define d2 (mkdisk "SlateGray"))
(define d3 (mkdisk "Orchid"))
(define row (ghost (rectangle 100 20)))
(define p (foldl vl-append (blank) (list (lc-superimpose row d1)
                                         (cc-superimpose row d2)
                                         (rc-superimpose row d3))))

