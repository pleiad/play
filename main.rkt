#lang racket

(require (except-in plai/datatype define-type)
        (except-in plai/test-harness test test/exn)
        "defmac.rkt"
        (for-syntax racket/syntax)
        (for-syntax racket)
        redex
        rackunit
        rackunit/text-ui)

(provide (except-out (all-from-out plai/datatype) type-case)
        (except-out (all-from-out racket) error (for-syntax error) #%module-begin)
        (except-out (all-from-out plai/test-harness) plai-error)
        (all-from-out redex)
        deftype
        defmac
        test-group
        test/exn
        (all-from-out rackunit) ;; TODO limit scope
        (all-from-out rackunit/text-ui) ;; TODO limit scope
        (rename-out [plai-error error]
                   [play-module-begin #%module-begin]
                   [match-define def]
                   [define defun]
                   [check-equal? test] ;; OVERRIDES PLAI's 'test' function with RackUnit's check-equal?
                   ))

;; TODO: macro 'test' to capture the test expression
(define-syntax (test-group stx)
  (syntax-case stx ()
    [(test-group suite-name test-expr ...)
     (with-syntax ()
       #'(module+ test
           (newline)
           (printf "Ejecutando tests: ~a" suite-name)
           (newline)
           (void (run-tests
                  (test-suite
                   suite-name
                   test-expr ...) 'verbose))))]))

(define-syntax test/exn
  (syntax-rules ()
    [(test/exn test-expr message)
     (check-exn (λ (e) (regexp-match (regexp message) (exn-message e))) (λ () test-expr))]))

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

;(define-syntax (

(define-syntax (deftype stx)
  (syntax-case stx ()
    [(deftype (t tfield ...) (variant vfield ...) ...)     
     (with-syntax* ([lang-name (format-id (syntax deftype) "RL-~a" #'t)]
                    [(lang-terms ...) (process-variant (syntax->list #'((variant vfield ...) ...)))]
                    [lang-parser-name (format-id (syntax deftype) "parse-redex-~a" #'t)]
                    [(lang-parser-subcalls ...) (process-variant-parser #'lang-parser-name (syntax->list #'((variant vfield ...) ...)))])       
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
           
           ;;========================================================================================
           ;; Definitions for exploiting Redex facilities
           
           ;; Defines the RL-<deftype-name> language for Redex
           ;; It defines a language with expressions 'e', where
           ;; each variant of the deftype is a variant of the possible expressions.
           (define-language lang-name               
             (e ::=
               lang-terms ...))
           
           ;; Defines the parse-redex-<deftype-name> function
           ;; for parsing a Redex-generated term into a deftype-based program.
           (define (lang-parser-name redex-sexp)
             (cond
               ;; Base cases for redex primitives
               [(number? redex-sexp) redex-sexp]    ;; for number natural integer real
               [(boolean? redex-sexp) redex-sexp]
               [(symbol? redex-sexp) redex-sexp]
               [(string? redex-sexp) redex-sexp]
               
               ;; Recursive cases for each of the non-terminals
               ;; in the language                 
               [(list? redex-sexp)
                (case (first redex-sexp)
                  lang-parser-subcalls ...)]))           
           ))]
    
    [(deftype t (variant vfield ...) ...)     
     (syntax (deftype (t) (variant vfield ...) ...))]))

(define-syntax (play-provide stx)
  (raise-syntax-error #f "The PLAY language provides all defined names" stx))

(define-syntax (play-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(#%module-begin
        (provide #,(datum->syntax stx '(all-defined-out)))
        body ...)]))