#lang racket

(provide (all-defined-out))

(define id (λ args args))
(define no (λ args (void)))
(define en no)
(define sp no)

;; active language
(set! sp id)
