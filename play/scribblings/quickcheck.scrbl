#lang scribble/manual

@(require scribble/eval
          scribble/bnf
          ;(for-label play)
          (for-label racket)
          )

@;(define ex-eval (make-base-eval))
@;interaction-eval[#:eval ex-eval (require play)]
@;interaction-eval[#:eval ex-eval (print-only-errors #t)]

@title[#:version ""]{QuickCheck}

@author{Ismael Figueroa}

@section[#:tag "tag1"]{Section 1}

@declare-exporting[#:use-sources (play/quickcheck)]

@defproc[(check [config config?] [prop property?]) void?]{
  Checks a property under the specified configuration.
}

@defstruct[sandwich ([protein ingredient?] [sauce ingredient?])]{
  A strucure type for sandwiches. Sandwiches are a pan-human foodstuff
  composed of a partially-enclosing bread material and various
  ingredients.
}