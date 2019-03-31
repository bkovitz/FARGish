; mkhtml.rkt -- Makes the fargish.html file

#lang racket

(require txexpr data/collection data/pvector (except-in sugar repeat))

(define html
  `(html
     (head (title "FARG model")
           (link ((rel "stylesheet") (type "text/css") (href "fargish.css"))))
     (body
       (svg ((id "ws") (width "960") (height "600")))
       (script ((src "jquery.js")))
       (script ((src "https://d3js.org/d3.v4.min.js")))
       (script ((src "force-directed3.js"))))))

(with-output-to-file "fargish.html" #:mode 'text #:exists 'replace
  (λ () (displayln (xexpr->html html))))
