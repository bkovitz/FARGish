; support.rkt -- Support network

#lang debug at-exp racket

(require "wheel.rkt")

(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

(struct support-config* (continuity-constant positive-feedback-rate normalize)
                        #:prefab)
; continuity-constant : number?  Decay rate: proportion of an element's support
;                                that persists from one timestep to the next
; positive-feedback-rate : number?
; normalize : (Hashof Any number?) -> (Hashof Any number?)
;                                Called after adding support to all elements

(define default-support-config
  (support-config* 0.9 0.2 identity))

; TODO
; normalize
; support-targets
; d-t
; special handling for antipathy

; i and j are keys for node/edges/whatever that have or give support. The
; variable i refers to a node that is giving support; the variable j refers
; to a node that is receiving support.

; si-t means "The support that i has on timestep t."

; config : support-config*
; support-targets : Any -> (Listof Any)  Elements that an element might
;                                        support or oppose
; d-t : Any Any -> number?               Amount of support given from first
;                                        element to second (negative for
;                                        antipathy)
; support-t : (Hashof Any number?)       Support levels at time t
;
; Returns (Hashof Any number?), the support levels at time t+1.
(define (support-t+1 config support-targets d-t support-t)
  (define alpha (support-config*-continuity-constant config))
  (define positive-feedback-rate
    (support-config*-positive-feedback-rate config))
  (define normalize (support-config*-normalize config))

  (define ht-total-input  ; total input support to j
    (for*/fold ([ht empty-hash])
               ([(i si-t) (in-hash support-t)]
                [j (support-targets i)])
      (define sj-t (hash-ref support-t j 0.0))
      (define positive-feedback-ij (* positive-feedback-rate sj-t))
      (define wij-t (+ (d-t i j) positive-feedback-ij))

      (hash-update ht
                   j
                   (λ (old) (+ old (* wij-t si-t)))
                   0.0)))

  (define xj-t (hash->numeric-f ht-total-input))

  (normalize
    (for/hash ([(j sj-t) support-t])
      (values j (+ (* alpha sj-t)
                   (* (- 1.0 alpha) (xj-t j)))))))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "simple test of support-t+1"
    (define support-0 (hash 'a 1.0
                            'b 0.0
                            'c 0.0))
    (define d-t (hash->numeric-f/arity-2 (hash '(a b) 1.0
                                               '(b c) 1.0
                                               '(c a) 1.0)))
    (define support-targets (hash->f (hash 'a '(b)
                                            'b '(c)
                                            'c '(a))))
    (define (next ss)  ; support levels for next timestep
      (support-t+1 default-support-config support-targets d-t ss))

    (let* ([t1 #R (next support-0)]
           [t2 #R (next t1)])
      (check-equal? (round-all/ut t1)
                    (hash 'a 0.9
                          'b 0.1
                          'c 0.0)))))
