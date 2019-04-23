#lang racket

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre))

(define-tokens value-tokens (IDENTIFIER))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-srcloc
      [(char-set "{}()") lexeme]
      [(:* (:or alphabetic numeric (char-set "!@#$%^&*-_+=|<>?/")))
       (token-IDENTIFIER (string->symbol lexeme))]
      [(:* whitespace) (token 'WHITESPACE lexeme #:skip? #t)]))
  (thunk (my-lexer ip)))

(define sip (open-input-string "nodeclass { blah  }"))
(define th (tokenize sip))
