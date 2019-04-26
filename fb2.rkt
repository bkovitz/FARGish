#lang debug racket

(require brag/support br-parser-tools/lex
         "fb.brag")

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

(define (p str)
  (parse (tokenize (open-input-string str))))
