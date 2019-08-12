#lang racket

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre))

(define-tokens value-tokens (IDENTIFIER STRING))

(define-empty-tokens keywords (BLAH))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-srcloc
      [(char-set "{}()[]") lexeme]
      [#\" (token-STRING (list->string (get-string-token input-port)))]
      ["blah" (token-BLAH)]
      [(:* (:or alphabetic numeric (char-set "!@$%^&*-_+=|<>?/")))
       (token-IDENTIFIER (string->symbol lexeme))]
      [(:* whitespace) (token 'WHITESPACE lexeme #:skip? #t)]))
  (thunk (my-lexer ip)))

(define sip (open-input-string "nodeclass { blah  }"))
(define th (tokenize sip))

(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [scheme-whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [initial (:or letter (char-set "!$%&*/:<=>?^_~@"))]
  [subsequent (:or initial digit (char-set "+-.@"))]
  [comment (:: #\; (:* (:~ #\newline)) #\newline)]
   
   
  ;; See ${PLTHOME}/collects/syntax-color/racket-lexer.rkt for an example of
  ;; using regexp macros to avoid the cut and paste.
  ;   [numR (:: prefixR complexR)]
  ;   [complexR (:or realR
  ;                (:: realR "@" realR)
  ;                (:: realR "+" urealR "i")
  ;                (:: realR "-" urealR "i")
  ;                (:: realR "+i")
  ;                (:: realR "-i")
  ;                (:: "+" urealR "i")
  ;                (:: "-" urealR "i")
  ;                (:: "+i")
  ;                (:: "-i"))]
  ;   [realR (:: sign urealR)]
  ;   [urealR (:or uintegerR (:: uintegerR "/" uintegerR) decimalR)]
  ;   [uintegerR (:: (:+ digitR) (:* #\#))]
  ;   [prefixR (:or (:: radixR exactness)
  ;               (:: exactness radixR))]
   
  [num2 (:: prefix2 complex2)]
  [complex2 (:or real2
                 (:: real2 "@" real2)
                 (:: real2 "+" ureal2 "i")
                 (:: real2 "-" ureal2 "i")
                 (:: real2 "+i")
                 (:: real2 "-i")
                 (:: "+" ureal2 "i")
                 (:: "-" ureal2 "i")
                 (:: "+i")
                 (:: "-i"))]
  [real2 (:: sign ureal2)]
  [ureal2 (:or uinteger2 (:: uinteger2 "/" uinteger2))]
  [uinteger2 (:: (:+ digit2) (:* #\#))]
  [prefix2 (:or (:: radix2 exactness)
                (:: exactness radix2))]
  [radix2 "#b"]
  [digit2 (:or "0" "1")]
  [num8 (:: prefix8 complex8)]
  [complex8 (:or real8
                 (:: real8 "@" real8)
                 (:: real8 "+" ureal8 "i")
                 (:: real8 "-" ureal8 "i")
                 (:: real8 "+i")
                 (:: real8 "-i")
                 (:: "+" ureal8 "i")
                 (:: "-" ureal8 "i")
                 (:: "+i")
                 (:: "-i"))]
  [real8 (:: sign ureal8)]
  [ureal8 (:or uinteger8 (:: uinteger8 "/" uinteger8))]
  [uinteger8 (:: (:+ digit8) (:* #\#))]
  [prefix8 (:or (:: radix8 exactness)
                (:: exactness radix8))]
  [radix8 "#o"]
  [digit8 (:/ "0" "7")]
   
  [num10 (:: prefix10 complex10)]
  [complex10 (:or real10
                  (:: real10 "@" real10)
                  (:: real10 "+" ureal10 "i")
                  (:: real10 "-" ureal10 "i")
                  (:: real10 "+i")
                  (:: real10 "-i")
                  (:: "+" ureal10 "i")
                  (:: "-" ureal10 "i")
                  (:: "+i")
                  (:: "-i"))]
  [real10 (:: sign ureal10)]
  [ureal10 (:or uinteger10 (:: uinteger10 "/" uinteger10) decimal10)]
  [uinteger10 (:: (:+ digit10) (:* #\#))]
  [prefix10 (:or (:: radix10 exactness)
                 (:: exactness radix10))]
  [radix10 (:? "#d")]
  [digit10 digit]
  [decimal10 (:or (:: uinteger10 suffix)
                  (:: #\. (:+ digit10) (:* #\#) suffix)
                  (:: (:+ digit10) #\. (:* digit10) (:* #\#) suffix)
                  (:: (:+ digit10) (:+ #\#) #\. (:* #\#) suffix))]
   
  [num16 (:: prefix16 complex16)]
  [complex16 (:or real16
                  (:: real16 "@" real16)
                  (:: real16 "+" ureal16 "i")
                  (:: real16 "-" ureal16 "i")
                  (:: real16 "+i")
                  (:: real16 "-i")
                  (:: "+" ureal16 "i")
                  (:: "-" ureal16 "i")
                  "+i"
                  "-i")]
  [real16 (:: sign ureal16)]
  [ureal16 (:or uinteger16 (:: uinteger16 "/" uinteger16))]
  [uinteger16 (:: (:+ digit16) (:* #\#))]
  [prefix16 (:or (:: radix16 exactness)
                 (:: exactness radix16))]
  [radix16 "#x"]
  [digit16 (:or digit (:/ #\a #\f) (:/ #\A #\F))]
   
   
  [suffix (:or "" (:: exponent-marker sign (:+ digit10)))]
  [exponent-marker (:or "e" "s" "f" "d" "l")]
  [sign (:or "" "+" "-")]
  [exactness (:or "" "#i" "#e")])
