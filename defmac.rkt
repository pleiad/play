#lang racket

;; defmac is like define-syntax-rule, except that it additionally supports 
;; two optional parameters. 
;; #:keywords <id> ... specifies keywords in the resulting macro
;; #:captures <id> ... specifies names that are inserted unhygienically
;; OOPLAI (http://www.dcc.uchile.cl/etanter/ooplai) uses defmac all along.

;; directly inspired by http://tmp.barzilay.org/defmac.ss by Eli Barzilay.
;; (this version is just a rewrite of Eli's, with a slightly different syntax
;; and using syntax-parse to handle the optional parameters)


(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax (defmac stx)
  (syntax-parse stx
    [(defmac (name:identifier . xs) 
       (~optional (~seq #:keywords key:identifier ...) #:defaults ([(key 1) '()]))
       (~optional (~seq #:captures cap:identifier ...) #:defaults ([(cap 1) '()]))
       body:expr)
     #'(define-syntax (name stx)
         (syntax-case stx (key ...)
           [(name . xs)
            (with-syntax ([cap (datum->syntax stx 'cap stx)] ...)
              (syntax body))]))]))