#lang racket/base

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require "../defmac.rkt")

(provide
 (all-from-out parser-tools/lex)
 (prefix-out @ (all-from-out parser-tools/lex-sre))
 define-lexer)

(define-syntax define-lexer
  (syntax-rules ()
    [(define-lexer lexer-name (pattern action) ...)
     (define lexer-name
       (lexer
        (pattern action) ...))]))

