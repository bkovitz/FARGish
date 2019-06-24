; yparse.rkt -- Experimental reader/parser/expander for y.brag

#lang debug at-exp racket

(require brag/support br-parser-tools/lex
         (only-in "y.brag" [parse bragparse])
         syntax/parse)

(define (tokenizer ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [(char-set ":;,.{}[]()=") lexeme] ; single-character tokens
      [(:or (char-set "+-*/<>") "==" "<=" "!=" ">=" "&&" "||")
       (token 'BINARY-OP lexeme)]
      ["--" lexeme]
      ["nodeclass" (token 'NODECLASS '(hash 'tag? #f))]
      ["tagclass" (token 'NODECLASS '(hash 'tag? #t))]
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
  (λ () (my-lexer ip)))

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

(define (->input-port src)
  (cond
    [(input-port? src) src]
    [(string? src) (open-input-string src)]
    [else (raise-arguments-error '->input-port
      "src cannot be converted to an input port; must be a string or already an input port"
      "src" src)]))

(define (tokenize src)
  (let ([th (tokenizer (->input-port src))])
    (let loop ([tok (th)])
      (displayln tok) ;DEBUG
      (when (not (eq? 'EOF (position-token-token tok)))
        (loop (th))))))

(define (parse src)
  (bragparse (tokenizer (->input-port src))))
