; fb2.rkt -- Experimental reader/parser/expander for fb.brag
;
; This file should probably become the reader and expander for FARGish.

#lang debug racket

(require brag/support br-parser-tools/lex
         (only-in "fb.brag" parse))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [(char-set ":;,.{}[]()=") lexeme] ; single-character tokens
      [(:or (char-set "+-*/<>") "==" "<=" "!=" ">=" "&&" "||")
       (token 'BINARY-OP lexeme)]
      ["--" lexeme]
      ["nodeclass" (token 'NODECLASS (hash))]  ; empty hash => not a tag
      ["tagclass" (token 'NODECLASS (hash 'tag? #t))]
      ["codelet" 'CODELET]
      ["given" 'GIVEN]
      ["make" 'MAKE]
      ["in" 'IN]
      ["want" 'WANT]
      ["me" 'ME]
      ["STUB" 'STUB]
      [#\" (token 'STRING (list->string (get-string-token input-port)))]
      [(:: identifier-initial (:* identifier-subsequent))
       (token 'IDENTIFIER (string->symbol lexeme))]
      [whitespace (return-without-pos (my-lexer ip))]
      [(eof) 'EOF]))
  (λ () #R (my-lexer ip)))

(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define-lex-abbrevs
  [identifier-punctuation (char-set "!@$%^&*-_+=|<>?/")]
  [identifier-initial (:or alphabetic identifier-punctuation)]
  [identifier-subsequent (:or identifier-initial numeric)])

(define (lx str)
  (let ([th (tokenize (open-input-string str))])
    (let loop ([tok (th)])
      (displayln tok)
      (when (not (eq? 'EOF (position-token-token tok)))
        (loop (th))))))

(define (p str)
  (parse (tokenize (open-input-string str))))

(define t0 "in a;")
(define t1 "in a; in b;")
(define t2 "in a; in { b }")
(define t3 "in a(x);")
(define t4 "in a(x); in b;")
(define t5 "in a(x : Integer); in b;")

(define s0 "nodeclass a;")

(define s1 "nodeclass a; nodeclass b;")

(define s2 "nodeclass number(n : Integer);")

(define s3 "
nodeclass number(n : Integer);

nodeclass blah;
")


;(define-syntax 
