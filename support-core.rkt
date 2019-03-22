; support-core.rkt -- Core functions for support network
;
; Redesigned a bit, too.
; TODO Replace support-network.rkt.

#lang typed/racket

(require "typed-wheel.rkt")

(provide support-ht-t+1)

(: sum-deltas
   (All (A)
     (-> A (Listof A))                        ; i->js
     (-> A A Flonum)                          ; i-j->give
     (Listof A)                               ; is
     -> (Hashof A Flonum)))
(define (sum-deltas i->js i-j->give is)
  (for*/fold ([ht : (Hashof A Flonum) (hash)])
             ([i (in-list is)]
              [j (i->js i)])
    (hash-update ht j + (const 0.0))))

; Applies alpha (continuity constant), minimum values, and normalization.
; Ensures that every i and j has an entry in the returned hash table.
(: post-proc
   (All (A)
     Flonum                                   ; alpha
     (-> (Hashof A Flonum) (Hashof A Flonum)) ; normalize
     (Listof A)                               ; is
     (-> A Flonum)                            ; i->min
     (-> A Flonum)                            ; i->support-t
     (Hashof A Flonum)                        ; ht/deltas
     -> (Hashof A Flonum)))
(define (post-proc alpha normalize is i->min i->support-t ht/deltas)
  (: update-sj-t+1 : (Hashof A Flonum) A Flonum -> (Hashof A Flonum))
  (define (update-sj-t+1 ht/sj-t+1 j delta)
    (let ([sj-t (i->support-t j)]
          [sj-t+1 (max (i->min j)
                       (+ (* alpha sj-t)
                          (* (- 1.0 alpha) delta)))])
      (hash-set ht/sj-t+1 j sj-t+1)))

  (let ([ht (for/fold ([ht : (Hashof A Flonum) (hash)]) ; support for the js
                      ([(j delta-j) ht/deltas])
              (update-sj-t+1 ht j delta-j))]
        [ht (for/fold ([ht : (Hashof A Flonum) ht]) ; support for any missing is
                      ([i is])
              (if (hash-has-key? ht i)
                ht
                (update-sj-t+1 ht i 0.0)))])
    (normalize ht)))

(: support-ht-t+1
   (All (A)
     Flonum                                   ; alpha
     (-> (Hashof A Flonum) (Hashof A Flonum)) ; normalize
     (-> A (Listof A))                        ; i->js
     (-> A A Flonum)                          ; i-j->give-support
     (-> A Flonum)                            ; i->min-support
     (-> A Flonum)                            ; i->support-t
     (Listof A)                               ; is
     -> (Hashof A Flonum)))
;Ensures that every i and j has an entry in the returned hash table. So, the
;returned hash table represents the complete support graph for time t+1.
(define (support-ht-t+1 alpha normalize i->js i-j->give-support i->min-support
                        i->support-t is)
  (post-proc alpha normalize is i->min-support i->support-t
             (sum-deltas i->js i-j->give-support is)))

;TODO UT
