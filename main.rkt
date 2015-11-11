#lang racket

(require (except-in plai/datatype define-type)
         plai/test-harness
         "defmac.rkt"
         (for-syntax racket/syntax)
         (for-syntax racket)
         redex)

(provide (except-out (all-from-out plai/datatype) type-case)
         (except-out (all-from-out racket) error (for-syntax error) #%module-begin)
         (except-out (all-from-out plai/test-harness) plai-error)
         (all-from-out redex)
         deftype
         defmac
         (rename-out [plai-error error]
                     [play-module-begin #%module-begin]                     
                     [match-define def]
                     [define defun]
                     ))

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
;; TODO: make it tail-recursive
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
                 (list (cons (car x) ;(syntax->datum (car x))
                             (for/list ([i (range 0 (length (syntax->list (cdr x))))])
                               #`(#,parser-name (car (repeat cdr redex-sexp #,(+ i 1)))))))))
         
         
         #|
                             (map (lambda (y)
                                    ;; There must be (list-pos+1) nested cdrs to redex-sexp
                                    ;; where list-pos is the position in the list, indexed from zero
                                    ;; That is (car (cdr redex-sexp)) (car (cdr (cdr ...))) (car (cdr (cdr (cdr ...))) ...
                                    #`(#,parser-name (car (cdr redex-sexp))))
                                  (syntax->list (cdr x)))))))
|#
         
         (map syntax-e variants-fields)))
  transformed-syntaxes)

(define-syntax (deftype stx)
  (syntax-case stx ()
    [(deftype (t tfield ...) (variant vfield ...) ...)
     (let ()
       (with-syntax* ([lang-name (format-id (syntax deftype) "RL-~a" #'t)]
                      [(lang-terms ...) (process-variant (syntax->list #'((variant vfield ...) ...)))]
                      [lang-parser-name (format-id (syntax deftype) "parse-redex-~a" #'t)]
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