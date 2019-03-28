#lang debug racket

(require web-server/servlet
         web-server/servlet-env)
 
(define count 0)

(define (my-app req)
  (set! count (add1 count))
  (let ([c (format "~a" count)])
    (response/xexpr
     `(html (head (title "Hello world!"))
            (body 
              (script ((src "jquery.js")))
              (script ((type "text/javascript"))
                "function do_button() {
                   $.get(\"count\",
                         (function (data) { $('#count').text(data) }))
                 }")
              (button ((id "1") (onClick "do_button()")) "Button")
              (p "Hey out there! " ,c)
              (p ((id "count")) ,c))))))

(define (req/count req)
  (response/output (λ (output-port)
                     (set! count (add1 count))
                     (write-string (number->string count) output-port)
                     (void))))
 
(define-values (x-dispatch x-url)
  (dispatch-rules
    [("standalone.rkt") my-app]
    [("count") req/count]))

(serve/servlet x-dispatch
               #:port 8080
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (current-directory)))
