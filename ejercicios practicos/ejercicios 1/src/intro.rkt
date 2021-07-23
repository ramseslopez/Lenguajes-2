#lang plai

;; Ejercicios prácticos 1
;; @author Ramses López
;; @date June 2021


;; Ejercicio 1
;; Procedimiento que eleva un número n a la potencia m
;; potencia :: number number --> number
(define (potencia n m)
  (cond
    [(equal? m 0) 1]
    [(< m 0) (/ 1 (* n (potencia n (- (* m -1) 1))))]
    [else (* n (potencia n (sub1 m)))]))

;; Ejercicio 2
;; Procedimiento que devueve una lista de los numeros menores o iguales a n
;; menores :: number -> (listof number)
(define (menores n)
  (cond
    [(< n 0) (error "Número no válido")]
    [(equal? n 0) (cons 0 '())]
    [else (concatena (menores (sub1 n)) (list n))]))

;; Ejercicio 3
;; Procedimiento que calcula la suma de los primeros n numeros naturales al cuadrado
;; suma-cuadradosR :: number -> number
(define (suma-cuadradosR n)
  (cond
    [(<= n 0) (error "Número no válido")]
    [(equal? n 1) 1]
    [else (+ (* n n) (suma-cuadradosR (sub1 n)))]))

;; Ejercicio 4
;; Procedimiento que devuelve un día de la semana acorde con un natural n
;; diaSemana :: number -> String
(define (diaSemana n)
  (cond
    [(equal? (modulo n 7) 0) "Lunes"]
    [(equal? (modulo n 7) 1) "Martes"]
    [(equal? (modulo n 7) 2) "Miércoles"]
    [(equal? (modulo n 7) 3) "Jueves"]
    [(equal? (modulo n 7) 4) "Viernes"]
    [(equal? (modulo n 7) 5) "Sábado"]
    [(equal? (modulo n 7) 6) "Domingo"]
    [else (error "Día fuera de rango")]))

;; Ejercicio 5
;; Predicado que verifica si un elemnto se encuentra en una lista
;; pertenece? :: any (listof any) --> boolean
(define (pertenece? e lst)
  (cond
    ;;[(null? e) (error "No hay elemento por buscar")]
    [(equal? (length lst) 0) #f]
    [(equal? e (car lst)) #t]
    [else (pertenece? e (cdr lst))]))

;; Ejercicio 6
;; Procedimiento que elimina elementos repetidos de una lista
;; eliminaRep :: (listof any) --> (listof any)
(define (eliminaRep lst)
  (match lst
    ['() lst]
    [(cons x xs) (cond
                   [(pertenece? x xs) (eliminaRep xs)]
                   [else (concatena (list x) (eliminaRep xs))])]))

;; Ejercicio 7
;; Procedimiento que devuelve la reversa de una lista
;; reversa-lista :: (listof any) --> (listof any)
(define (reversa-lista lst)
  (cond
    [(empty? lst) lst]
    [else (concatena (reversa-lista (cdr lst)) (list (car lst)))]))

;; Ejercicio 8
;; Predicado que verifica si una lista es palíndroma
;; palindromo-lista? :: (listof any) --> boolean
(define (palindromo? lst)
  (cond
    [(empty? lst) #t]
    [else (equal? lst (reversa-lista lst))]))

;; Ejercicio 9
;; Procedimiento que devuelve el elemento máximo de una lista
;; maximo :: (listof number) --> number
(define (maximo lst)
  (cond
    [(empty? lst) (error "La lista no tiene elementos")]
    [else (last (sort lst <))]))

;; Ejercicio 10
;; Procedimiento que devuelve la lista de divisores de un número
;; divisores :: number --> (listof number)
(define (divisores n)
  (reversa-lista (concatena (list n) (div n (sub1 n)))))

;; Definición del tipo Punto
(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 11
;; Procedimiento que calcula el punto medio entre dos puntos
;; punto-medio :: Punto Punto --> Punto
(define (punto-medio p q)
  (cond
    [(and (Punto? p) (Punto? q)) (let* ([x1 (get-x p)] [y1 (get-y p)] [x2 (get-x q)] [y2 (get-y q)])
                                   (punto (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))]
    [else (error "Acción no válida")]))

;; Ejercicio 12
;; Procedimiento para calcular la distancia entre dos puntos
;; distancia :: Punto Punto --> number
(define (distancia p q)
  (cond
    [(and (Punto? p) (Punto? q)) (let* ([x1 (get-x p)] [y1 (get-y p)] [x2 (get-x q)] [y2 (get-y q)])
                                   (sqrt (+ (potencia (- x2 x1) 2) (potencia (- y2 y1) 2))))]
    [else (error "Acción no válida")]))

;; Ejercicio 13
;; Tipos para figuras geométricas

;; Definición del tipo Círculo
(define-type Circulo
  [circulo (centro Punto?) (radio positive-integer?)])

;; Definición del tipo Triángulo
(define-type Triangulo
  [triangulo (punto1 Punto?) (punto2 Punto?) (punto3 Punto?)])

;; Definición del tipo Cuadrado
(define-type Cuadrado
  [cuadrado (pizq Punto?) (longitud positive-integer?)])

;; Definición del tipo Rectángulo
(define-type Rectangulo
  [rectangulo (pizq Punto?) (base positive-integer?) (altura positive-integer?)])

;; Ejercicio 14
;; Procedimiento que devuelve el elemento más frecuente de una lista
;; masRepetido :: (listof any) --> any
(define (masRepetido lst)
  (cond
    [(empty? lst) (error "La lista no tiene elementos")]
    [else (let ([pairs (list-veces lst)])
            (fst pairs (snd pairs)))]))

;; Ejercicio 15
;; Procedimiento que devuelve el producto cruz de una lista consigo misma
;; conjunto-cuadrado :: (listof number) --> (listof (pairof number))
(define (conjunto-cuadrado lst)
  (cond
    [(empty? lst) '()]
    [else (producto-cruz lst lst)]))

;; Ejercicio 16
;; Procedimiento que calcula el cambio que tenemos que devovler según el
;; monto a cobrar y el monto pagado. Devuelve la cantidad de monedas de las
;; denominaciones $50, $20, $10, $5, $2, $1
;; cambio :: number number --> (listof number)
(define (cambio total pago)
  (let ([lst (list 50 20 10 5 2 1)])
  (cond
    [(equal? total pago) (list 0 0 0 0 0 0)]
    [else (denominaciones (- pago total) lst)])))


;; Ejercicio 17
;; Procedimiento que calcula la descomposición en factores primos de un número
;; descomposicion-primos :: number -> (listof (pairof number))
(define (descomposicion-primos n)
  (list-veces (factores n (primos-lista n))))

;; Ejercicio 18
;; Tipo para árbol biario de búsqueda (ABB)
(define-type ABB
  [vacio]
  [hoja (n number?)]
  [nodo (r number?) (izq ABB?) (der ABB?)])

;; Ejercicio 19
;; Procedimiento que agrega un elemento al árbol
;; agrega :: number ABB --> ABB
(define (agrega x tree)
  (match tree
    [(vacio) (nodo x (vacio) (vacio))]
    [(nodo r izq der) (cond
                        [(< x r) (nodo r (agrega x izq) der)]
                        [(> x r) (nodo r izq (agrega x der))]
                        [else (error "El elemento ya está contenido en el árbol")])]))

;; Ejercicio 20
;; Procedimeinto que verifica si un elemento está contenido en el árbol
;; contiene :: ABB number --> boolean
(define (contiene tree x)
  (match tree
    [(vacio) #f]
    [(hoja n) (cond
                [(equal? x n) #t]
                [else #f])]
    [(nodo r izq der) (cond
                        [(equal? x r) #t]
                        [(contiene izq x) #t]
                        [(contiene der x) #t]
                        [else #f])]))




;; =========== Funciones auxiliares =============== ;;

;; Procedimiento que concatena dos listas
;; concatena :: (listof any) (listof any) --> (listof any)
(define (concatena l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (concatena (cdr l1) l2))]))

;; Predicdo que verifica si un numero es divisor de otro
;; divisor? :: number number --> boolean
(define (divisor? m n)
  (equal? (modulo n m) 0))

;; Procedimiento que devuelve la lista de divisores de un número a partir de su antecesor
;; div :: number number --> (listof number)
(define (div n m)
  (cond
    [(< n m) (error "No es posible hacer esta acción")]
    [(equal? m 0) '()]
    [(divisor? m n) (concatena (list m) (div n (sub1 m)))]
    [else (div n (sub1 m))]))

;; Procedimiento  para obtener la coordenada x de un punto
;; get-x :: Punto -> number
(define (get-x p)
  (match p
    [(punto x y) x]))

;; Procedimiento  para obtener la coordenada y de un punto
;; get-y :: Punto -> number
(define (get-y p)
  (match p
    [(punto x y) y]))

;; Procedimiento para contar las veces que aparece un elemento en una lista
;; n-veces :: any (listof any) --> number
(define (n-veces x lst)
  (match lst
    ['() 0]
    [(cons y ys) (cond
                   [(equal? x y) (+ 1 (n-veces x ys))]
                   [else (n-veces x ys)])]))

;; Procedimeinto que muestra el numero de veces que aparece un elemento junto con el elemento en sí
;; lista-veces :: (listof any) --> (listof (pairof any))
(define (list-veces lst)
  (cond
    [(empty? lst) '()]
    [else (eliminaRep (map (lambda (y) (list y (n-veces y lst))) lst))]))

;; Procedimiento para calcular el producto cruz de dos listas
;; producto-cruz :: (listof number) (listof number) --> (listof (pairof number))
(define (producto-cruz lst1 lst2)
  (match lst1
    ['() '()]
    [(cons x xs) (concatena (map (lambda (y) (list x y)) lst2) (producto-cruz xs lst2))]))

;; Procedimiento que verifica si un número es primo
;; primo? :: number --> boolean
(define (primo? n)
  (cond
    [(equal? n 1) #f]
    [else (equal? (divisores n) (list 1 n))]))

;; Procedimiento que devuelve una lista de primos hasta un número dado
;; primos-lista :: number --> (listof number)
(define (primos-lista n)
  (filter primo? (cdr (menores n))))

;; Procedimiento que obtiene los factores primos de un números
;; factores :: number --> (listof number)
(define (factores n lst)
  (cond
    [(equal? n 1) '()]
    [(equal? (modulo n (car lst)) 0) (cons (car lst) (factores (quotient n (car lst)) lst))]
    [else (factores n (cdr lst))]))

;; Procedimiento que calcula las denominaciones de un número
;; denominaciones :: number (listof number) --> (listof number)
(define (denominaciones red l)
  (cond
    [(empty? l) '()]
    [(<= (quotient red (car l)) 0) (cons 0 (denominaciones red (cdr l)))]
    [else (append (list (quotient red (car l))) (denominaciones (modulo red (car l)) (cdr l)))]))

;; Procedimiento que obtiene el primer miembro de mayor frecuencia de una lista de pares
;; fst :: (listof (pairof any)) number --> any
(define (fst lst n)
  (match lst
    ['() 0]
    [(cons x xs) (cond
                   [(equal? (second x) n) (first x)]
                   [else (fst xs n)])]))

;; Procedimiento que obtiene el segundo elemento de mayor frecuencua de una lista de pares
;; snd :: (listof (pairof any)) --> number
(define (snd lst)
  (match lst
    ['() 0]
    [(cons x xs) (cond
                   [(> (second x) (snd xs)) (second x)]
                   [else (snd xs)])]))
