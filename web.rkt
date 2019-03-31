; web.rkt -- Simple web server for FARGish graphical user interface

; 31-Mar-2019 Does nothing but serve files from current directory.

#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define-values (x-dispatch x-url)
  (dispatch-rules))

(serve/servlet x-dispatch
  #:port 8080
  #:launch-browser? #f
  #:servlet-regexp #rx""
  #:extra-files-paths (list (current-directory)))
