; Throwaway code to explore suspending and restarting

#lang debug at-exp racket/gui

(require describe)

(define suspend? (void)) (set! suspend? #f)
(define resume #f)    ; holds continuation to resume

;(define saved-n (void))

(define (loop n)
  ;(maybe-suspend (λ () (set! saved-n n)))
  (maybe-suspend n)
  (if (< n 5)
    (loop (add1 n))
    n))

#;(define (maybe-suspend f)
  (when suspend?
    (let/cc k
      (f)
      (set! resume (λ ()
                     (set! resume #f)
                     (k)))
      (abort-current-continuation (default-continuation-prompt-tag)
                                  (const 'suspended)))))

(define (maybe-suspend v)
  (when suspend?
    (let/cc k
      (set! resume (λ ()
                     (set! resume #f)
                     (k)))
      (abort-current-continuation (default-continuation-prompt-tag)
                                  (const v)))))

(define (go)
  (if resume
    (resume)
    (loop 0)))
