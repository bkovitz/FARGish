#lang debug at-exp racket

(require web-server/servlet
         web-server/servlet-env
         txexpr data/collection data/pvector (except-in sugar repeat))

(define (write-html html filename)
  (with-output-to-file filename #:mode 'text #:exists 'replace
    (λ ()
      (displayln "<!DOCTYPE HTML plus SVG>")
      (displayln (xexpr->html html)))))

(define html
  `(html
     (head (title "Demo of force-directed layout")
           (link ((rel "stylesheet") (type "text/css") (href "fargish.css"))))
     (body
       (svg ((id "ws") (width "960") (height "600")))
       (script ((src "jquery.js")))
       (script ((src "https://d3js.org/d3.v4.min.js")))
       (script ((src "force-directed3.js"))))))

