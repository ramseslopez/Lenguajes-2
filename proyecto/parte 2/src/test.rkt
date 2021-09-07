#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))
(require (file "./interp.rkt"))
(require (file "./verifier.rkt"))


;; Función auxiliar para pruebas, llama a parse, desugar y luego interp
;; con el contexto de tipos vacío.
;; test: SRCFWBAE-Typed? -> SRCFWBAE-Value
(define (pruebaInterp x)
  (interp (desugar (parse x)) (mtSub)))


;; Función auxiliar para pruebas, llama a parse y a typeof
;; con el contexto de tipos vacío.
;; test: SCFWBAE -> Type
(define (pruebaTypeOf exp)
  (typeof (parse exp) (phi)))


(display "PRUEBAS TYPEOF\n______________________________________________________________________________________________________________________________\n\n")

(test (pruebaTypeOf '#t) (booleanT))

(test (pruebaTypeOf '2) (numberT))

(test (pruebaTypeOf '{+ 1 2}) (numberT))

(test/exn (pruebaTypeOf '{+ 1 #f}) "typeof: Error in parameter (boolS #f)\nExpected type: (numberT)\nGiven type: (booleanT)")

(test/exn (pruebaTypeOf '{and 1 #f}) "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)")
                  
(test (pruebaTypeOf '{if #t 2 3}) (numberT))

(test (pruebaTypeOf '{with {{x : number 2} {y : boolean #t} {z : number 1}} {if y x z}}) (numberT))

(test (pruebaTypeOf '{with* {{x : number 2} {y : boolean #t} {z : number 1}} {if y x z}}) (numberT))

(test/exn (pruebaTypeOf '{if #t 2 #t}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (pruebaTypeOf '{cond {#t 2} {#f 3} {else #t}}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (pruebaTypeOf '{if 3 2 #t}) "if: Type error\nConditional's test-expr type must be a boolean\nGiven: (numberT)")

(test (pruebaTypeOf '{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}}) (funT (list (numberT) (booleanT) (numberT))))

(test/exn (pruebaTypeOf '{fun {{x : number} {y : number}} : (number number -> number) {if y x 0}}) "if: Type error\nConditional's test-expr type must be a boolean\nGiven: (numberT)")

(test (pruebaTypeOf '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 #t}}) (numberT))

(test/exn (pruebaTypeOf '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 3}})
          "app: Type error:\nParameter's type doesn't match argument's type.\nExpected: (booleanT)")

(test (pruebaTypeOf
     '{rec ([fac : (number -> number) {fun {(n : number)} : (number -> number)  {if {zero? n} 1 {* n {fac ({- n 1})}}}}]
            [n : number 5])
        {fac (n)}}) (numberT)) 

(display "PRUEBAS INTERP\n______________________________________________________________________________________________________________________________\n\n")

(test (pruebaInterp '3) (numV 3))

(test (pruebaInterp #t) (boolV #t))

(test/exn (pruebaInterp 'x) "lookup: Hay un identificador libre: x")

(test (pruebaInterp '{if {< 1 1} 5 6}) (numV 6))

(test (pruebaInterp '{if {< 1 2} 5 6}) (numV 5)) 

(test/exn (pruebaInterp '{if {fun [{x : number}] : (number -> number) {+ x 1}} 5 6}) "interp: Símbolo no esperado. La condicional de if, no es un booleano")

(test (pruebaInterp '{car (lst 1 2 3)}) (numV 1))

(test (pruebaInterp '{car {with ([a : string "hola"]) (lst a #t 4)}}) (stringV "hola"))

(test (pruebaInterp '{append (lst #\a #\b #\c) (lst 1 2 3)})  (listV (list (charV #\a) (charV #\b) (charV #\c) (numV 1) (numV 2) (numV 3))))

(test/exn (pruebaInterp '{car 2}) "interp: Símbolo no esperado. El parámetro de car debe ser una lista.")

(test (pruebaInterp'{cond {(= 2 2) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 5))

(test (pruebaInterp'{cond {(= 2 4) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 6))

(test (pruebaInterp'{cond {(= 2 4) 5} {#f 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 7))

(test (pruebaInterp'{cond {(= 2 4) 5} {#f 6} {(< 3 3) 7} {#t 8} {else 9}}) (numV 8))

(test (pruebaInterp'{cond {(= 2 4) 5} {#f 6} {(< 3 3) 7} {#f 8} {else 9}}) (numV 9))

(test (pruebaInterp '{string-append "ho" "la"}) (stringV "hola"))

(test (pruebaInterp '{cons 3 (lst 1 2)}) (listV (list (numV 3) (numV 1) (numV 2))))

(test (pruebaInterp '{append (lst 1 2) (lst 1 2 3)}) (listV (list (numV 1) (numV 2) (numV 1) (numV 2) (numV 3))))

(test (pruebaInterp '{length (lst 1 2)}) (numV 2))

(test (pruebaInterp '{with ([a : number 3] [b : number 2]) (+ a b)}) (numV 5))

(test (pruebaInterp '{with {{x : number 5} {y : number 1}} {+ x y}}) (numV 6))

(test/exn (pruebaInterp '{with {{x : number 5} {y : number {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x")

(test (pruebaInterp '{with {{f : (number -> number) {fun {[x : number]} : (number -> number) {+ x x}}}} {f {3}}}) (numV 6)) 

(test/exn (pruebaInterp '{with {{x : number 3} {f : (number -> number) {fun {[a : number]} : (number -> number) {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")


(test (pruebaInterp '{with* {{x : number 5} {y : number 1}} {+ x y}}) (numV 6)) 

(test (pruebaInterp '{with* {{x : number 5} {y : number {+ x 1}}} {+ x y}}) (numV 11))

(test (pruebaInterp '{with* {{x : number 3}}
                      {with* {{f : (number -> number) {fun {[y : number]} : (number -> number)  {+ x y}}}}
                             {with* {{x : number 4}}
                                    {f {1}}}}}) (numV 4)) 
(test (pruebaInterp '{with* {{x : number 1} {y : number 2} {z : number 3}}
                      {fun {[x : number] [y : number] [z : number]} : (number number number -> number) {+ x {+ y z}}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (numV 3) (aSub 'y (numV 2) (aSub 'x (numV 1) (mtSub))))))

(test (pruebaInterp '{with* {{x : number 3}
                             {f : (number -> number) {fun {[a : number]} : (number -> number) {+ x a}}}}
                            {f {0}}}) (numV 3))

(test (pruebaInterp '{{fun {[x : number] [y : number]} : (number number -> number)
                           {+ x y}}
                      {10 3}}) (numV 13)) 


(test (pruebaInterp '{with {{x : number 1}
                            {y : number 2}}
                           {with* {{z : number 3}
                                   {w : number z}}
                                  {with {{f : (number -> number) {fun {[x : number]} : (number -> number) x}}}
                                        {f {w}}}}})
      (numV 3))

(test (pruebaInterp '{with* {{x : number 5}
                       {w : number {+ x 1}}
                       {z : number {with {{x : number 10}
                                          {f : (number -> number) {fun {[a : number]} : (number -> number) {+ x a}}}}
                                         {f {10}}}}}
                            {+ x z}}) (numV 20))

(test (pruebaInterp
     '{rec ([fac : (number -> number) {fun {(n : number)} : (number -> number)  {if {zero? n} 1 {* n {fac ({- n 1})}}}}]
            [n : number 5])
        {fac (n)}}) (numV 120))

(test/exn (pruebaInterp '{with* [(f : (number -> number)
                                    (fun {(n : number)} : (number -> number)
                                         (expt n 2)))
                                 (m : number (+ (f (6)) 45) )]
                                {/ (f (4)) m #\v}}) "interp: El operador sólo acepta números")

(test (pruebaInterp '{with* [(f : (number -> number)
                                (fun {(n : number)} : (number -> number)
                                     (* n 8)))
                             (t : number (with [(p : number (f (6)))] {+ p 6}))
                             (m : number 20)]
                            {+ (f (2)) (expt t m)}}) (numV 44450351179593105816204799588171792))

(test (pruebaInterp '{with* [(f : (list -> list)
                                (fun {(l : list)} : (list -> list)
                                     (cdr l)))
                             (c : number (* 5 5))
                             (x : number (modulo 167 c))]
                            {expt c (+ x (car (f ((lst 34 56 1 34 0 8)))))}})
      (numV 1121038771459853656738983666631932905024209553501212617405654627111832866148688481189310550689697265625))

(test (pruebaInterp
     '{rec ([fibo : (number -> number)
                  {fun {(n : number)} : (number -> number)
                       {if {zero? n}
                           0
                           {if {= n 1}
                               1
                               {+ (fibo {(- n 1)}) (fibo {(- n 2)})}}}}]
            [n : number 12])
        {fibo (n)}}) (numV 144))

(test (pruebaInterp
     '{rec ([fibo-tail : (number number number -> number)
                  {fun {(n : number) (acc1 : number) (acc2 : number)} : (number number number -> number)
                       {if {zero? n}
                           acc2
                           {if {= n 1}
                               acc2
                               (fibo-tail {(- n 1) acc2 (+ acc1 acc2)})}}}]
            (n : number 10) (acc1 : number 0) (acc2 : number 1))
        {fibo-tail (n acc1 acc2)}}) (numV 55))

(test (pruebaInterp
     '{rec ([long : (list -> number)
                  {fun {(l : list)} : (list -> number)
                       {if {empty? l}
                           0
                           {+ 1 {long {(cdr l)}}}}}]
            [l : list (lst 1 2 3 4 5)])
        {long (l)}}) (numV 5))

(test (pruebaInterp
     '{rec ([rev : (list -> list)
                 {fun {(l : list)} : (list -> list)
                      {if {empty? l}
                          ()
                          {append {rev {(cdr l)}} (lst (car l))}}}]
            [l : list (lst 2 4 6 8 10)]) {rev (l)}}) (listV (list (numV 10) (numV 8) (numV 6) (numV 4) (numV 2))))