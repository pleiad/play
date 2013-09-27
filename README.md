`#lang play`
====

Variations on `#lang plai` (the language that accompanies [PLAI](http://cs.brown.edu/~sk/Publications/Books/ProgLangs/)).

We create `#lang plai` in the [Pleiad lab](http://pleiad.cl) to teach Programming Languages (with [PLAI](http://cs.brown.edu/~sk/Publications/Books/ProgLangs/) and [OOPLAI](http://www.dcc.uchile.cl/etanter/ooplai)),
but it can obviously be used for general Racket programming.

# What's in there?

### From `#lang plai`, we keep the testing support:

- `test` 
- `test/exn` 
- `print-only-errors`


### We introduce several new forms:

- `deftype` to define structures with a concise syntax that directly corresponds to the constructor syntax

```racket
(deftype Expr
  (num n)
  (add l r))
```

- `def` to define identifiers, possibly using pattern matching

```racket
(def x 10)
(def (cons a b) (get-some-cons)) ;; pattern matching, defines a and b at once
```
- `defun` as an alias for define (for the convenient function definition syntax)

```racket
(defun (interp expr)
  ....)
```

- `defmac` as an extended version of `define-syntax-rule`, which supports keywords and captures

```racket
(defmac (for from low to high in bodies ...)
  #:keywords from to in
  #:captures it
  ....)
```

### How does it look in practice?

For observing `def`, `defun` and `deftype` in action, see [various interpreters from PLAI](http://www.dcc.uchile.cl/etanter/play-interps/) rewritten in `#lang play`.

For uses of `defmac`, see [OOPLAI](http://www.dcc.uchile.cl/etanter/ooplai), an OOP-with-macros supplement to PLAI.

# Installation

In DrRacket (v5.3.6 or newer) go to File > Install Package > `play`
