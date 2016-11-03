#lang racket

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require parser-tools/yacc)
(require "../defmac.rkt")

(provide
 (all-from-out parser-tools/lex)
 (prefix-out @ (all-from-out parser-tools/lex-sre)) 
 define-lexer
 consume-lexer
 (all-from-out parser-tools/yacc)
 define-parser
 parse
 )


(define-syntax define-lexer
  (syntax-rules ()
    [(define-lexer lexer-name (pattern action) ...)
     (define lexer-name
       (lexer
        (pattern action) ...))]))


;; lexer x input-port -> list[lexer-values]
(define (consume-lexer l input)
  (match (l input)
    ['eof '()]
    [lexer-value (cons lexer-value (consume-lexer l input))]))


(define-syntax define-parser
  (syntax-rules ()
    [(define-parser parser-name clause ...)
     (define parser-name
       (parser
        clause ...))]))

(define-syntax parse
  (syntax-rules (with-parser and-lexer)
    [(parse program with-parser parser and-lexer lexer)
     (let ((input-program (open-input-string program)))
       (parser (lambda () (lexer input-program))))]))
     


