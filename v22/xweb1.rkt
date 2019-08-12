; xweb1.rkt -- More elementary experimentation with serving data over the
;              web from Racket

#lang debug at-exp racket

(require web-server/servlet
         web-server/servlet-env
         txexpr data/collection data/pvector (except-in sugar repeat))
 
(define count 0)

; Like response/xexpr but doesn't escape the bodies of 'script' and 'style'
; tags. How does anyone live without this?
(define (response/x xexpr)
  (response/output (λ (output-port)
                     (write-string (xexpr->html xexpr) output-port)
                     (void))))

(define (left-to-right ls)
  (for/fold ([pv (pvector)]
             [x 0]
             #:result (sequence->list pv))
            ([elem ls])
    (values (conj pv (attr-set elem 'x x))
            (+ x 2 (->int (attr-ref elem 'width 10))))))

(define my-svg
  `(svg
     ,@(left-to-right (make-list 10 `(rect ((width "20") (height "20")))))))

(define (my-app req)
  (set! count (add1 count))
  (let ([c (format "~a" count)])
    (response/x
     `(html (head (title "Hello world!"))
            (body 
              (style "
.links line {
  stroke: #999;
  stroke-opacity: 0.6;
}

.nodes circle,rect {
  stroke: #fff;
  stroke-width: 1.5px;
}

text {
  font-family: sans-serif;
  font-size: 10px;
}")
              (svg ((width "960") (height "600")))
              (script ((src "jquery.js")))
              (script ((src "https://d3js.org/d3.v4.min.js")))
              (script ((type "text/javascript") (src "force-directed2.js")))
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
