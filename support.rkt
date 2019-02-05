; support.rkt -- Support network

#lang debug at-exp racket

(require "wheel.rkt")

(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

(provide support-t+1 support-config default-support-config
         normalize-by-reverse-sigmoid)

;; ======================================================================
;;
;; Configuration struct
;;

(struct support-config* (continuity-constant positive-feedback-rate)
                        #:prefab)
; continuity-constant : number?  Decay rate: proportion of an element's support
;                                that persists from one timestep to the next
; positive-feedback-rate : number?

(define support-config support-config*)

(define default-support-config
  (support-config* 0.9 0.2))

; TODO
; normalize
; support-targets
; d-t
; special handling for antipathy
; self-support

; i and j are keys for node/edges/whatever that have or give support. The
; variable i refers to a node that is giving support; the variable j refers
; to a node that is receiving support.

; si-t means "The support that i has on timestep t."

; config : support-config*
; normalize : (Hashof Any number?) -> (Hashof Any number?)
;                                Called after adding support to all elements
; support-targets : Any -> (Listof Any)  Elements that an element might
;                                        support or oppose
; d-t : Any Any -> number?               Amount of support given from first
;                                        element to second (negative for
;                                        antipathy)
; support-t : (Hashof Any number?)       Support levels at time t
;
; Returns (Hashof Any number?), the support levels at time t+1.
(define (support-t+1 config normalize support-targets d-t support-t)
  (define alpha (support-config*-continuity-constant config))
  (define positive-feedback-rate
    (support-config*-positive-feedback-rate config))

  (define xj-t  ; total input support to j
    (hash->numeric-f
      (for*/fold ([ht empty-hash])
                 ([(i si-t) (in-hash support-t)]
                  [j (support-targets i)])
        (define sj-t (hash-ref support-t j 0.0))
        (define positive-feedback-ij (* positive-feedback-rate sj-t))
        (define wij-t (+ (d-t i j) positive-feedback-ij))

        (hash-update ht
                     j
                     (λ (old) (+ old (* wij-t si-t)))
                     0.0))))

  (normalize
    (for/hash ([(j sj-t) support-t])
      (values j (+ (* alpha sj-t)
                   (* (- 1.0 alpha) (xj-t j)))))))

;; ======================================================================
;;
;; Functions related to normalization
;;

; Makes an even function out of f. The returned function returns f(x)
; for x > 0, -f(-x) for x < 0, 0.0 for x = 0.
; f : number? -> number?
(define (evenf f)
  (λ (x)
    (cond
      [(positive? x) (f x)]
      [(negative? x) (- (f (- x)))]
      [else 0.0])))

; Sum of absolute values
(define (+abs . args)
  (for/sum ([arg args])
    (abs arg)))

; Scales all the xs so that the largest absolute value is 1.0.
(define (unit-scaled xs)
  (cond
    [(empty? xs) xs]
    [else
      (let ([scaling-factor (/ (apply max (map abs xs)))])
        (map (curry * scaling-factor) xs))]))

; Sigmoid function that wraps around y=x for 0.0 <= x <= 1.0.
; p : number?     Exponent: 2.0 for classic sigmoid where values close to
;                 0.0 are moved closer to 0.0 and values close to 1.0 are
;                 moved closer to 1.0. Setting p=0.5 makes a reverse
;                 sigmoid, where values move toward 0.5.
; x : number?
(define (diag-sigmoid p x)
  (/ (expt x p)
     (+ (expt x p)
        (expt (- 1 x) p))))

(define reverse-sigmoid (evenf (curry diag-sigmoid 0.5)))

; If the sum of the absolute values of the xs > upper-bound-on-sum, returns
; xs rescaled via reverse-sigmoid. Otherwise returns xs unchanged.
(define (rescale-by-reverse-sigmoid upper-bound-on-sum xs)
  (define initial-sum (apply +abs xs))
  (cond
    [(<= initial-sum upper-bound-on-sum)
     xs]
    [else
      (define new-xs (map reverse-sigmoid (unit-scaled xs)))
      (define new-sum (apply +abs new-xs))
      (define scaling-factor (/ upper-bound-on-sum new-sum))
      (map (curry * scaling-factor) new-xs)]))

(define (normalize-by-reverse-sigmoid upper-bound-on-sum ht)
  (zip-hash (hash-keys ht)
            (rescale-by-reverse-sigmoid upper-bound-on-sum (hash-values ht))))

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
      (support-t+1 default-support-config identity support-targets d-t ss))

    (let* ([t1 (next support-0)]
           [t2 (next t1)])
      (check-equal? (round-all/ut t1)
                    (hash 'a 0.9
                          'b 0.1
                          'c 0.0))))
  
  (test-case "test of competitive normalization with reverse-sigmoid"
    (define support-0 (hash 'a 1.0
                            'b 1.0
                            'c 1.0))
    (define d-t (hash->numeric-f/arity-2 (hash '(a b) 2.0
                                               '(b c) 1.0
                                               '(c a) 1.0)))
    (define support-targets (hash->f (hash 'a '(b)
                                           'b '(c)
                                           'c '(a))))
    ; ensure that sum of all support does not exceed 3.0
    (define normalize (curry normalize-by-reverse-sigmoid 3.0))

    (define (next ss)  ; support levels for next timestep
      (support-t+1 default-support-config normalize support-targets d-t ss))

    (let* ([t1 (next support-0)])
      (check-equal? (round-all/ut t1)
                    (hash 'a 0.9055
                          'b 1.1890
                          'c 0.9055)))
  ))
