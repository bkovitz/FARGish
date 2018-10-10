; Throwaway code to explore suspending and restarting

#lang debug at-exp racket

(require rackunit describe)

(provide suspended maybe-suspend suspend? set-suspend?!)

(define suspend? #f)
(define suspend-k (void))

(define (set-suspend-k! susp-k)
  (set! suspend-k susp-k))

(define (set-suspend?! bool)
  (set! suspend? bool))

(define (maybe-suspend v)
  (if suspend? (suspend-k v) v))

(define-syntax-rule (suspended expr)
  (letrec ([make-resume-k
             (λ (there v)  ; v is the last value from expr via (suspend-k v)
               (λ ()
                 (let/cc here
                   (set-suspend-k! (make-suspend-k here))
                   (there v))))]  ; give v back to expr
           [make-suspend-k
             (λ (here)
               (λ (v)   ; expr calls (suspend-k v)
                 (let/cc there
                   (set! resume-k (make-resume-k there v))
                   (here v))))]  ; return v to caller/controller
           [first-call
             (λ ()
               (let/cc here
                 (set-suspend?! #t)
                 (set-suspend-k! (make-suspend-k here))
                 expr
                 (set-suspend?! #f)
                 (let loop ()
                   (suspend-k (void))
                   (loop))))]
           [resume-k first-call])
    (λ ()
      (resume-k))))

(module+ test
  (test-case "maybe-suspend"
    (define (func n)
      (let loop ([i 0])
        (if (< (maybe-suspend i) n)
          (loop (add1 i))
          i)))

    (define run-to-next-x (suspended (func 4)))

    (define results '())

    (define (get-next)
      (set! results (cons (run-to-next-x) results)))

    (get-next) ;0
    (get-next) ;1
    (get-next) ;2
    (get-next) ;3
    (get-next) ;4
    (get-next) ;void
    (get-next) ;void
    (get-next) ;void

    (check-equal? (reverse results)
                  (list 0 1 2 3 4 (void) (void) (void)))))
