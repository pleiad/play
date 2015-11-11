#lang racket

(require play)

;;
;; In addition to the AE structs and variants
;; the new deftype defines a Redex language
;; named RL-AE, or more generally, "RL-<your deftype name>"

(deftype AE  
  [num number]        ;; we must/can use redex primitive value generators for base cases:
  ;; for now we support: number, natural, integer, real, string, boolean
  [bool boolean]  
  [add l r])

;; Check it out
(display RL-AE)

(define (interp-ae an-ae)
  (match an-ae
    [(num n) n]
    [(bool b) b]    
    [(add l r) (+ (interp-ae l) (interp-ae r))]))

;; We can generate random terms using Redex facilities
;; The name of the expressions metavariables is always 'e'
(generate-term RL-AE e 3)

;; However this may generate ill-defined terms such as
;; (add (num 0) (bool #f))
;; which is perfectly fine since this language is untyped

;; But we can do better by extending the language with a (as simple as you want) type system
;; This is also done using Redex facilities
;; For instance:

;; Define extended RL-AE-T language,
;; adding Tnum and Tbool types to the original RL-AE language
(define-extended-language RL-AE-T RL-AE
  (t ::= Tnum Tbool))

;; We define a simple type system
(define-judgment-form
  RL-AE-T
  #:mode (types I O)
  #:contract (types e t)
  
  [-----------------
   (types (num number) Tnum)]
  
  [-----------------
   (types (bool boolean) Tbool)]
  
  [(types e_1 Tnum)
   (types e_2 Tnum)
   -----------------
   (types (add e_1 e_2) Tnum)])

;; We can now generate an arbitrary well-typed term
(define well-typed-redex-term (generate-term RL-AE-T #:satisfying (types e t) 3))

;; The actual term is in the second position of the generated value...
(define actually-well-typed-redex-term (second well-typed-redex-term))

;; And now it would be very desirable to have this term expressed
;; in the 'real' deftype-based structrus
;; in order to have a proper AST for our interpreters..
;; Thankfully, deftype also defines a function 'parse-redex-<your deftype name>'
;; which performs this parsing!!

(define well-typed-ae-term (parse-redex-AE actually-well-typed-redex-term))

;; And of course, we can now interpret it as usual!
(interp-ae well-typed-ae-term)

;; This opens the door for automated testing of student interpreters vs reference implementations
;; and hopefully many other applications :)
;; For instance if you combine it with racket-quickcheck:

#|
(require quickcheck)

(define random-prog-property
  (property ([n arbitrary-natural])
            (define prog (second (generate-term RL-AE-T #:satisfying (types e t) (+ n 1))))       
            (display (interp-ae (parse-redex-AE prog)))
            (newline)
            ;; Test if student submission is observationally-equivalent to reference implementation
            ;; (equal? (reference-interp prog) (student-inter prog))            
            (= 1 1)))

;(quickcheck random-prog-property)
|#
