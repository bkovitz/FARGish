; xsusp3.rkt -- Run a function as a coroutine that occasionally yields control 
;               with a tagged result

#lang debug at-exp racket

(require rackunit describe)

(provide suspended maybe-suspend suspend? set-suspend?!)

(define suspend? #f)
(define suspend-k (void))

(define (set-suspend-k! susp-k)
  (set! suspend-k susp-k))

(define (set-suspend?! bool)
  (set! suspend? bool))

(define (maybe-suspend tag v)
  (if suspend?
    (begin (suspend-k tag v) v)
    v))

(define-syntax-rule (suspended expr)
  (letrec ([make-resume-k
             (λ (there)  ; there = continuation in expr
               (λ ()
                 (let/cc here
                   (set-suspend-k! (make-suspend-k here))
                   (there))))]
           [make-suspend-k
             (λ (here)  ; here = continuation in controller
               (λ (tag v)   ; expr calls (suspend-k tag v)
                 (let/cc there
                   (set! resume-k (make-resume-k there))
                   (here (list tag v)))))]  ; return `(,tag ,v) to controller
           [first-call
             (λ ()
               (let/cc here
                 (set-suspend?! #t)
                 (set-suspend-k! (make-suspend-k here))
                 (let ([result expr])
                   (set-suspend?! #f)
                   (let loop ()
                     (suspend-k 'done result) ; return this after expr is done
                     (loop)))))]
           [resume-k first-call])
    (λ ()
      (resume-k))))

(module+ test
  (test-case "maybe-suspend"

    (define (func n)
      (let loop ([i 0])
        (maybe-suspend 'start-of-loop i)
        (if (< (maybe-suspend 'i i) n)
          (loop (add1 i))
          i)))

    (define run-to-next-x (suspended (func 4)))

    (define results '())

    (define (get-next)
      (set! results (cons (run-to-next-x) results)))

    (get-next) ;(start-of-loop 0)
    (get-next) ;(i 0)
    (get-next) ;(start-of-loop 1)
    (get-next) ;(i 1)
    (get-next) ;(start-of-loop 2)
    (get-next) ;(i 2)
    (get-next) ;(start-of-loop 3)
    (get-next) ;(i 3)
    (get-next) ;(start-of-loop 4)
    (get-next) ;(i 4)
    (get-next) ;(done 4)
    (get-next) ;(done 4)
    (get-next) ;(done 4)

    (check-equal? (reverse results)
                  `((start-of-loop 0)
                    (i 0)
                    (start-of-loop 1)
                    (i 1)
                    (start-of-loop 2)
                    (i 2)
                    (start-of-loop 3)
                    (i 3)
                    (start-of-loop 4)
                    (i 4)
                    (done 4)
                    (done 4)
                    (done 4)))))
