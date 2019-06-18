#lang racket

(require (except-in plai/datatype define-type)
         plai/test-harness
         "./defmac.rkt"
         (for-syntax racket/syntax)
         (for-syntax racket)
         redex)

(provide deftype-redex
         (all-from-out redex))

(define-for-syntax (process-variant variants-fields)
  ;; From http://docs.racket-lang.org/redex/The_Redex_Reference.html
  ;; Only allow: number, natural, integer, real, string, boolean
  (define transformed-syntaxes
    (map (lambda (x)
           (cons (syntax->datum (car x))
                (map (lambda (y)
                       (if (member (syntax->datum y) '(number natural integer real string boolean variable))
                          (syntax->datum y)
                          (syntax->datum #'e)))
                    (syntax->list (cdr x)))))         
        (map syntax-e variants-fields)))  
  transformed-syntaxes)

;; n nested applications of 'f' over argument x
;; That is, (f (f ... (f x)))
(define (repeat f x n)
  (define (inner-repeat f x n acc)  
    (if (= n 1)
       (f acc)
       (inner-repeat f x (- n 1) (f acc))))
  (inner-repeat f x n x))

;; Creates terms of the form
;; ((variant) (variant (parser-name (car redex-sexpr))
;;                     (parser-name (car (cdr redex-sexpr))
;;                     (parser-name (car (cdr (cdr redex-sexpr))) ...) ...
;; According to the number of variants and the number of vfields in each variant
;; This is then used in the define lang-parser-name function.
(define-for-syntax (process-variant-parser parser-name variants-fields)
  (define transformed-syntaxes    
    (map (lambda (x)
           (cons (list (syntax->datum (car x)))
                (list (cons (car x)
                           (for/list ([i (range 0 (length (syntax->list (cdr x))))])
                             #`(#,parser-name (car (repeat cdr redex-sexp #,(+ i 1)))))))))         
        (map syntax-e variants-fields)))
  transformed-syntaxes)

(define-syntax (deftype-redex stx)
  (syntax-case stx ()
    [(deftype-redex (t tfield ...) (variant vfield ...) ...)
     (let ()
       (with-syntax* ([lang-name (format-id (syntax deftype-redex) "RL-~a" #'t)]
                      [(lang-terms ...) (process-variant (syntax->list #'((variant vfield ...) ...)))]
                      [lang-parser-name (format-id (syntax deftype-redex) "parse-redex-~a" #'t)]
                      [(lang-parser-subcalls ...)
                       (process-variant-parser                        
                        #'lang-parser-name
                        (syntax->list #'((variant vfield ...) ...)))])
         #'(begin             
             ;; Define the root variant t with all its applicable global fields
             ;; We also override the constructor so only variants can be constructed
             (struct t (tfield ...)
               #:transparent #:mutable
               #:guard 
               (λ (tfield ... const)
                 (if (eq? const 't) 
                     (error (format "cannot construct value of type ~a: use one of the variants ~a"
                                    't (list 'variant ...)))
                     (values tfield ...))))
             
             ;; Define all variants of the root structure t
             (struct variant t (vfield ...) #:transparent #:mutable) ...
             ;; Define redex reification
             
             (define-language lang-name
               ;; How to add as many e's as variant ... there are
               ;; How to add 'primitive' redex values? like number, boolean, etc..?
               ;; Hopefully using optional/keyword arguments
               (e ::=
                  lang-terms ...))
             
             (define (lang-parser-name redex-sexp)
               (cond
                 ;; Base cases for redex primitives
                 [(number? redex-sexp) redex-sexp]
                 [(boolean? redex-sexp) redex-sexp]
                 [(symbol? redex-sexp) redex-sexp]
                 [(string? redex-sexp) redex-sexp]
                 
                 ;; Recursive cases for each of the non-terminals
                 ;; in the language                 
                 [(list? redex-sexp)
                  (case (first redex-sexp)
                    lang-parser-subcalls ...)]))           
             )))]
    
    [(deftype-redex t (variant vfield ...) ...)     
     (syntax (deftype-redex (t) (variant vfield ...) ...))]))
