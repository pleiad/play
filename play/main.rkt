#lang racket

(require (except-in plai/datatype define-type)
         plai/test-harness
         "../defmac.rkt"
         (for-syntax racket/syntax)
         (for-syntax racket)
         (except-in rackunit check)
         rackunit/text-ui)

(provide (except-out (all-from-out plai/datatype) type-case)
         (except-out (all-from-out racket) error (for-syntax error) #%module-begin)
         (except-out (all-from-out plai/test-harness) plai-error)         
         deftype
         defmac
         check/exn
         (all-from-out rackunit)
         (all-from-out rackunit/text-ui)
         (rename-out [plai-error error]
                     [play-module-begin #%module-begin]                     
                     [match-define def]
                     [define defun]
                     [check-equal? check] ;; OVERRIDES PLAI's 'test' fnction with RackUnit's 'check-equal?'
                     ))

(define-syntax check/exn
  (syntax-rules ()
    [(check/exn test-expr message)
     (check-exn (λ (e) (regexp-match (regexp message) (exn-message e))) (λ () test-expr))]))

(define-syntax deftype
  (syntax-rules ()
    [(deftype (t tfield ...) (variant vfield ...) ...)
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
    [(deftype t (variant vfield ...) ...)
     (deftype (t) (variant vfield ...) ...)]))


(define-syntax (play-provide stx)
  (raise-syntax-error #f "The PLAY language provides all defined names" stx))

(define-syntax (play-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(#%module-begin
        (provide #,(datum->syntax stx '(all-defined-out)))
        body ...)]))