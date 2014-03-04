#lang racket

(require (except-in plai/datatype define-type)
         plai/test-harness
         "defmac.rkt")

(provide (except-out (all-from-out plai/datatype) type-case)
         (except-out (all-from-out racket) error (for-syntax error) #%module-begin)
         (except-out (all-from-out plai/test-harness) plai-error)
         deftype
         defmac
         (rename-out [plai-error error]
                     [play-module-begin #%module-begin]                     
                     [match-define def]
                     [define defun]
                     ))

(define-syntax deftype
  (syntax-rules ()
    [(define-type (t tfield ...) (variant vfield ...) ...)
     (begin
       (struct t (tfield ...)
         #:transparent #:mutable
         #:guard 
         (λ (tfield ... const)
           (if (eq? const 't) 
               (error (format "cannot construct value of type ~a: use one of the variants ~a"
                              't (list 'variant ...)))
               (values tfield ...))))
       (struct variant t (vfield ...) #:transparent #:mutable) ...)]
    [(define-type t (variant vfield ...) ...)
     (define-type (t) (variant vfield ...) ...)]))

(define-syntax (play-provide stx)
  (raise-syntax-error #f "The PLAY language provides all defined names" stx))

(define-syntax (play-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(#%module-begin
        (provide #,(datum->syntax stx '(all-defined-out)))
        body ...)]))