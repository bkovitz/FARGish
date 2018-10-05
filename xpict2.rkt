; Throwaway code to see how to display spreading activation graphically

#lang debug at-exp racket/gui

(require pict pict/color data/collection data/pvector)

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

(define (make-layout activations)
  (define ts (map text (activations->archetype-names activations)))
  (define max-width (apply max (map pict-width ts)))
  (define max-height (apply max (map pict-height ts)))
  (define awidth (+ max-width 10.0))
  (define aheight (+ max-height 20.0))
  (define cell-ghost (ghost (rectangle awidth aheight)))
  (define cells (map (λ (t)
                       (inset
                         (cb-superimpose
                           (ct-superimpose cell-ghost t)
                           (disk 15 #:draw-border? #f #:color "Aquamarine"))
                         0.0 10.0))
                     ts))
  (define layout-width 900.0)
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
