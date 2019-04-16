; web.rkt -- Simple web server for FARGish graphical user interface

; 31-Mar-2019 Does nothing but serve files from current directory.

#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/rerequire)

; QUESTION How do you force "x32.rkt" to get reloaded if it's changed?
(require (only-in "x32.rkt" step/web write-graph/json))

;(define g : (U Graph Void) (void))
(define g (void))
(set! g g)

(define (step-model! req)
  (set! g (step/web g))
  (response/output (λ (output-port)
                     (write-graph/json g output-port)
                     (void))))

(define (reset-model! req)
  (set! g (void))
  (step-model! req))

(define-values (x-dispatch x-url)
  (dispatch-rules
    [("step") step-model!]
    [("reset") reset-model!]))

(serve/servlet x-dispatch
  #:port 8080
  #:launch-browser? #f
  #:servlet-regexp #rx""
  #:extra-files-paths (list (current-directory)))
