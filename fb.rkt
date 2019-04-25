#lang racket

(require br-parser-tools/lex
         br-parser-tools/yacc
         (prefix-in : br-parser-tools/lex-sre))

(define-tokens value-tokens (IDENTIFIER STRING))
(define-empty-tokens keywords
  (GIVEN MAKE EOF LEFT-BRACE RIGHT-BRACE LEFT-BRACKET RIGHT-BRACKET
   LEFT-PAREN RIGHT-PAREN))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [#\{ 'LEFT-BRACE]
      [#\} 'RIGHT-BRACE]
      [#\[ 'LEFT-BRACKET]
      [#\] 'RIGHT-BRACKET]
      [#\( 'LEFT-PAREN]
      [#\) 'RIGHT-PAREN]
      [#\" (token-STRING (list->string (get-string-token input-port)))]
      ["given" 'GIVEN]
      ["make" (token-MAKE)]
      [(:: identifier-initial (:* identifier-subsequent))
       (token-IDENTIFIER (string->symbol lexeme))]
      [whitespace (return-without-pos (my-lexer ip))]
      [(eof) 'EOF]))
  (thunk (my-lexer ip)))

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
      (when (not (eq? 'EOF (srcloc-token-token tok)))
        (loop (th))))))

(define parse
  (parser 
    (src-pos)
    (start start)
    (end EOF)
    (tokens keywords value-tokens)
    (error (λ args (displayln args)))
    (grammar
      (start [(GIVEN LEFT-BRACE RIGHT-BRACE MAKE LEFT-BRACE RIGHT-BRACE)
              (+ 1 1)] ))))

(define (p str)
  (parse (tokenize (open-input-string str))))

