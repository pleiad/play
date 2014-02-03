#lang scribble/manual

@(require scribble/eval
          scribble/bnf)

@(define ex-eval (make-base-eval))
@interaction-eval[#:eval ex-eval (require play)]
@interaction-eval[#:eval ex-eval (print-only-errors #t)]

@title[#:version ""]{El Lenguaje PLAY}

@author{Éric Tanter}

Racket es a la vez un lenguaje y una plataforma para definir varios lenguajes y
sus extensiones, permitiendo construir programas a partir de módulos definidos
en múltiples lenguajes. Por ejemplo, este documento está escrito en el lenguaje
@(hyperlink "http://docs.racket-lang.org/scribble/index.html" "Scribble"), una
especie de LaTeX moderno adecuado para documentar programas. El documento
@(hyperlink "http://www.dcc.uchile.cl/~etanter/ooplai/" "OOPLAI"), que usaremos
para estudiar OOP en el curso, también fue escrito en Scribble. Otros lenguajes
que fueron definidos con Racket incluyen lenguajes pedagógicos, un Racket con
tipos estáticos, Datalog para programación lógica, un lenguaje para definir
aplicaciones web, un lenguaje para especificar lenguajes formalmente, y muchos
más...

El curso de lenguajes no explora esta faceta de Racket, pero sí usaremos
variantes de Racket. El curso usa principalmente el lenguaje PLAY, que está
basado en el lenguaje PLAI. PLAI agrega esencialmente funciones convenientes
para especificar tests y para definir y manejar estructuras de datos.

PLAY reutiliza los mecanismos para tests de PLAI, y simplifica el uso de pattern
matching sobre las estructuras de datos, de forma que sea análogo al caso de
matching sobre listas. Además posee una sintáxis más sucinta para definir valores, basándose también en el uso de pattern matching.

Para instalar el lenguaje PLAY debe utilizar la versión más reciente de
DrRacket, e instalar el paquete play: File > Install Package > @code{github://github.com/ifigueroap/play/master}

@section[#:tag "def1"]{Definiendo Identificadores}
PLAY introduce la forma @scheme[def] que permite introducir identificadores de forma simple o utilizando pattern matching.
En el primer caso, @scheme[def] se utiliza de la misma forma que @scheme[define]. Por ejemplo:

@racketblock[
(def x-max 100)
(def y-max 100)
]

 El segundo caso se explica en @secref["def2"].

@section[#:tag "defun2"]{Definiendo Funciones}

Recuerde que hay dos maneras de definir funciones con @scheme[define]:

@interaction[#:eval ex-eval
 (define (f x) (+ x x))
 (define f (λ (x) (+ x x)))
 ]

Solamente podemos usar @scheme[def] para definir funciones como en la segunda línea. La primera forma provoca un conflicto con la sintáxis para pattern matching:
@interaction[#:eval ex-eval
 (def (f x) (+ x x))
 (def f (λ (x) (+ x x)))
 ]

Para mantener la conveniencia de la primera forma de definir funciones, PLAY introduce @scheme[defun]:
;
@interaction[#:eval ex-eval
 (defun (f x) (+ x x))
]

En este caso usar @scheme[defun] es equivalente a usar @scheme[define] para el caso específico de definir funciones.

@;{
La única diferencia radica en que @scheme[define] provee una forma @hyperlink{http://docs.racket-lang.org/reference/begin.html @scheme[begin]} implícita, la que permite 
evaluar un conjunto de expresiones en orden. Por ejemplo, usando @scheme[define]:

@def+int[#:eval ex-eval
 (define (f x)
   (printf "Argumento es ~a" x)
   (+ x x))
 (f 1)           
]

El equivalente usando @scheme[defun] es:

@def+int[#:eval ex-eval
 (defun (f x)
   (begin
     (printf "Argumento es ~a" x)
     (+ x x)))
 (f 1)              
]

El origen de esta limitación es un pequeño conflicto de sintáxis, que proviene del hecho que @scheme[defun] combina el uso de @scheme[define] con una forma sucinta para definir funciones basadas en pattern matching, que se explica a continuación.
}
  
@section[#:tag "def2"]{Definiciones Basadas en Patrones}
Cuando desarrollamos funciones que involucran varios cómputos intermedios, es una buena práctica
introducir identificadores para referirse a esos valores, y así mejorar la legibilidad del código.

Por ejemplo, considere que representamos puntos como un par de coordenadas. Podemos calcular la distancia entre dos puntos como:

@racketblock+eval[#:eval ex-eval
 (code:comment "distance :: (cons Int Int) (cons Int Int) -> Float")
 (code:comment "Calcula la distancia entre dos puntos")
 (defun (distance p1 p2)   
   (def x1 (car p1))
   (def x2 (car p2))
   (def y1 (cdr p1))
   (def y2 (cdr p2))
   (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
]

@interaction[#:eval ex-eval
 (distance (cons 0 0) (cons 1 1))
 (distance (cons 3 4) (cons 9 0))
]

Aquí utilizamos @scheme[def] para nombrar las coordenadas de cada 
punto, de la misma forma que usaríamos @scheme[define]. Sin embargo, 
acceder a los valores de una estructura arbitraria puede ser bastante 
engorroso. Utilizando una definición basada en patrones se 
simplifica bastante el código:

@racketblock+eval[#:eval ex-eval
(code:comment "distance :: (cons Int Int) (cons Int Int) -> Float")
(code:comment "Calcula la distancia entre dos puntos")
(defun (distance p1 p2)  
  (def (cons x1 y1) p1)
  (def (cons x2 y2) p2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
]
@interaction[#:eval ex-eval
 (distance (cons 0 0) (cons 1 1))
 (distance (cons 3 4) (cons 9 0))
 ]

@;{
Es posible simplificar aún más el código, aprovechando la capacidad de @scheme[defun] para
definir directamente funciones basadas en pattern matching:

@racketblock+eval[#:eval ex-eval
(code:comment "distance :: (cons Int Int) (cons Int Int) -> Float")
(code:comment "Calcula la distancia entre dos puntos")
;(defun (distance p1 p2)
;  [((cons x1 y1) (cons x2 y2))
;   (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))])
]
@interaction[#:eval ex-eval
 (distance (cons 0 0) (cons 1 1))
 (distance (cons 3 4) (cons 9 0))
]

Al usar @scheme[defun] de esta forma es preciso asignar un patrón para cada argumento de la función. Al igual que @scheme[match], es posible especificar varias cláusulas según se requiera. Por ejemplo, considere una función optimizada de distancia entre puntos que chequea si los puntos están en la misma posición sobre alguno de los ejes:

@racketblock+eval[#:eval ex-eval
; (code:comment "distance :: (cons Int Int) (cons Int Int) -> Float")
;(code:comment "Calcula la distancia entre dos puntos")
;(defun (distance p1 p2)
;  [((cons x y) (cons x y)) 0]
;  [((cons x1 y1) (cons x1 y2)) (abs (- y1 y2))]
;  [((cons x1 y1) (cons x2 y1)) (abs (- x1 x2))]
;  [((cons x1 y1) (cons x2 y2))   
;   (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))])
]
@interaction[#:eval ex-eval
(distance (cons 9 9) (cons 9 9))
(distance (cons 3 2) (cons 3 5))
(distance (cons 2 3) (cons 5 3))
(distance (cons 3 4) (cons 9 0))
]
   
Observe que si usamos el mismo identificador en distintas posiciones de los constructores estamos en efecto
requiriendo que los valores sean iguales en dichas posiciones---esto permite simplificar mucho el código!
}
  
@margin-note{@scheme[def] es un alias de @hyperlink["http://docs.racket-lang.org/reference/match.html#(form._((lib._racket/match..rkt)._match-define))" @code{match-define}].}
              
@;{
El uso de @scheme[defun] para definir directamente una función basada en patrones corresponde a usar @hyperlink{"http://docs.racket-lang.org/reference/match.html#(form._((lib._racket/match..rkt)._define/match))" @scheme[define/match]}.
}


Las formas @scheme[def] y @scheme[defun] están diseñadas para producir código conciso y fácil de leer, para que así usted pueda enfocarse en lo esencial del curso. A pesar de basarse en otras formas estándar de Racket, es preferible que utilice estas expresiones. Así, además de los anteriores beneficios, su código sera robusto respecto a potenciales futuros cambios al lenguaje PLAY.

@section[#:tag "tests"]{Tests}

El lenguaje PLAY introduce @scheme[test] para verificar que una expresión evalúa al resultado esperado:
@interaction-eval[#:eval ex-eval (print-only-errors #f)]
@interaction[#:eval ex-eval
(test (+ 1 2) 3)
(test (+ 1 2) 4)
]

Además, el lenguaje PLAY introduce @scheme[test/exn] para testear por errores: 

@def+int[#:eval ex-eval
(defun (mysqrt x)
  (if (and (number? x) (positive? x))
      (sqrt x)
      (error "mysqrt: should be called with positive number")))      

(test/exn (mysqrt -1) "positive")
]
@interaction-eval[#:eval ex-eval (print-only-errors #t)]

El segundo argumento de @scheme[test/exn] tiene que ser un sub-string del mensaje de error esperado.

Es importante notar que @scheme[text/exn] solo detecta errores reportados explícitamente por nuestro código usando @scheme[error], como en el ejemplo anterior. No detecta errores lanzados implicitamente por el runtime del lenguaje:
@interaction[#:eval ex-eval
(test/exn (/ 1 0) "by zero") 
]

Por defecto PLAY muestra mensajes de éxito o fallo de los tests. Para mostrar mensajes sólo para los test fallidos, se debe evaluar:

@racketblock[
(print-only-errors #t)             
]

La evaluación de @scheme[print-only-errors] afecta todas las expresiones que le siguen, y en general basta con
utilizarla al principio del archivo.

@section[#:tag "datos"]{Estructuras de datos}

Racket incluye una forma básica de definir nuevas estructuras de datos. El
lenguaje PLAY provee una forma más elaborada y práctica, que usaremos en el
curso. Se llama @scheme[deftype]. Con @scheme[deftype], uno introduce
un nombre para la estructura de datos, y luego especifica uno o más variantes
de este tipo de dato. Por ejemplo:

@margin-note{@scheme[deftype] en conjunto con el uso de pattern matching (@scheme[match] y @scheme[def])
               reemplazan la funcionalidad de @scheme[define-type] y @scheme[type-case] provistos por el lenguaje PLAI}

@racketblock+eval[#:eval ex-eval
(deftype BinTree
  (leaf value)
  (node value left right))
]

Note que cada variante (en este caso @scheme[leaf] y @scheme[node]) pueden
tener distintas cantidades de atributos (incluso ninguno). Cada atributo se
define simplemente con un nombre (por ej. @scheme[value]).

Cuando se usa @scheme[deftype], se generan automaticamente varias
funciones. Primero, se crean constructores, uno por variante, que permiten
construir datos:

@interaction[#:eval ex-eval
(leaf 10)
(node 10 (leaf 3) (node 4 (leaf 1) (leaf 2)))
]

También se generan accesores, para inspeccionar datos existentes. Los accesores
tienen el nombre @emph{variante}@tt{-}@emph{atributo}, por ejemplo:

@interaction[#:eval ex-eval
(leaf-value (leaf 10))
(def y (node 1 (leaf 2) (leaf 3)))
(node-right y)
(leaf-value (node-left y))
]

Finalmente, se generan predicados de tipo, para poder clasificar datos respecto al tipo general y sus variantes:
@interaction[#:eval ex-eval
(BinTree? 10)
(BinTree? (leaf 10))
(BinTree? '(10))
(BinTree? (node 10 (leaf 1) (leaf 2)))
(leaf? 10)
(leaf? (leaf 10))
(node? '(1 2 3))
(node? (node 3 (leaf 1) (leaf 2)))
(node? (leaf 5))
]

Para hacer un análisis por casos de una estructura definida usando
@code{deftype}, simplemente utilizamos pattern matching sobre cada uno de
sus constructores o variantes. Por ejemplo:

@def+int[#:eval ex-eval
(defun (contains? bt n)
  (match bt
    [(leaf v) (equal? v n)]
    [(node v l r) (or (equal? v n)
    	       	      (contains? l n)
		      (contains? r n))]))

(contains? (leaf 1) 2)
(contains? (node 10 (leaf 3) (node 4 (leaf 1) (leaf 2))) 1)
]

@margin-note{El lenguaje Python soporta una una versión limitada de asignación basada en patrones, para el caso particular de @hyperlink["http://docs.python.org/3/tutorial/datastructures.html#tuples-and-sequences" "tuplas y listas"].}

También podemos usar @scheme[def] para acceder a los componentes
de la estructura:

@interaction[#:eval ex-eval
(def y (leaf 3))             
(def (node v l r) (node 1 (leaf 2) y))             
v
l
r
]


Note que a diferencia de la representación de árboles binarios usando listas
del capítulo anterior, ahora no debemos preocuparnos del orden en que van los
patrones correspondientes a los constructores de cada variante de la
estructura.

La razón de fondo es que en una estructura de datos definida inductivamente
usando @code{deftype} se cumple que:

@itemlist[
   @item{Los constructores son funciones inyectivas. Por ejemplo @code{(equal? (leaf a) (leaf b))} evalúa a @code{#t} solamente si @code{(equal? a b)} es @code{#t}.}
   @item{Valores construidos con distintos constructores nunca son iguales.}
   @item{No existe otra manera de construir valores de la estructura si no es con las variantes definidas.}
]

Por lo tanto, si seguimos la receta de diseño y hacemos matching solamente
sobre las variantes de una estructura, no tendremos que preocuparnos de
matchings accidentales o inesperados.

La única precaución que debemos tener es recordar ser exhaustivos en el
análisis de casos, para asegurarnos que las funciones sean completas para el
tipo de datos considerado.

@; Uno de los beneficios de @scheme[type-case] es que requiere que uno sea exhaustivo en el análisis de casos, asegurando que las funciones sean completas para el tipo de dato considerado:
@; @interaction[#:eval ex-eval
@; (define (contains? bt n)
@;   (type-case BinTree bt
@;     (leaf (v) (equal? v n))))
@; ]
@; Como ven, el error es reportado como un error de sintáxis: eso significa que la verificación tiene lugar antes de ejecutar el programa.

@bold{Ejercicios:} Defina las funciones @scheme[map-tree] y @scheme[sum-tree]
que operan sobre @scheme[BinTree]s.

@section{Un último ejemplo}

A modo de último ejemplo, considere un mini-lenguaje para describir expresiones aritméticas con números, adición y substracción: 

@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @nonterm{expr}
              @BNF-seq[open @litchar{num} @nonterm{number} close]
              @BNF-seq[open @litchar{add} @nonterm{expr} @nonterm{expr} close]
              @BNF-seq[open @litchar{sub} @nonterm{expr} @nonterm{expr} close])])
                                      
Note que la definición BNF nos da la gramática del lenguaje o, en forma equivalente, una descripción de la estructura de los árboles de sintáxis abstracta. Podemos describir el tipo de dato @scheme[Expr] directamente con @scheme[deftype]:

@racketblock+eval[#:eval ex-eval
(deftype Expr
  (num n)
  (add left right)
  (sub left right))
]

Un procesamiento simple de esta estructura es @emph{evaluar} una expresión para obtener su valor númerico. Por ejemplo, podríamos representar la expresión @scheme[1 + (5 - 2)] con la siguiente estructura:
@racketblock[
(add (num 1) 
     (sub (num 5) (num 2)))
]
y usar @scheme[calc] para obtener su valor:
@racketblock[
(calc (add (num 1) 
           (sub (num 5) (num 2))))
]

La definición de @scheme[calc] es directa, siguiendo la receta que estudiamos para procesar estructuras de datos (ver @secref{receta}). Primero, derivamos el patrón de @scheme[calc] basándonos directamente en la definición del tipo de dato @scheme[Expr]:
@racketblock[
(defun (calc expr)  
  (match expr
    [(num n) ...]
    [(add l r) ... (calc l) ... (calc r) ...]
    [(sub l r) ... (calc l) ... (calc r) ...]))
]
Lo cual es muy simple de completar:
@racketblock+eval[#:eval ex-eval
(code:comment "calc :: Expr -> number")
(code:comment "retorna el valor de la expresión dada")
(defun (calc expr)  
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]))

(test (calc (add (num 1) 
                 (sub (num 5) (num 2))))
      4)
]
Note como @scheme[deftype] y @scheme[match] calzan perfectamente con esta metodología.