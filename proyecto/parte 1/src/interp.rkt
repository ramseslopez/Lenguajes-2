#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup :: symbol DefrdSub --> CFWBAE
;; (define (lookup name ds)
(define (lookup name ds)
  (match ds
    [(mtSub) (error 'lookup "Identificador libre")]
    [(aSub id value env) (cond
                           [(equal? id name) value]
                           [else (lookup name env)])]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp :: CFWBAE DefrdSub --> CFWBAE-Value
(define (interp expr ds)
  (match expr
    [(id i) (lookup i ds)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(chaR chr) (charV chr)]
    [(strinG str) (stringV str)]
    [(lisT xs) (listV (map (lambda (x) (interp x ds)) xs))]
    [(iF cnd then els) (cond
                         [(equal? cnd (bool #t)) (interp then ds)]
                         [(equal? cnd (bool #f)) (interp els ds)]
                         [else (interp (iF (bool (boolV-b (interp cnd ds))) then els) ds)])]
    [(op f lst) (let* ([a (map (lambda (x) (cond
                                             [(numV? (interp x ds)) (numV-n (interp x ds))]
                                             [(boolV? (interp x ds)) (boolV-b (interp x ds))]
                                             [(charV? (interp x ds)) (charV-c (interp x ds))]
                                             [(stringV? (interp x ds)) (stringV-s (interp x ds))]
                                             [(listV? (interp x ds)) (map (lambda (y) (get-param y)) (listV-l (interp x ds)))])) lst)]
                                [b (apply f a)])
                              (cond
                                [(number? b) (numV b)]
                                [(boolean? b) (boolV b)]
                                [(char? b) (charV b)]
                                [(string? b) (stringV b)]
                                [(list? b) (listV (map (lambda (z) (interp (to-cfwbae z) ds)) b))]))]
    [(fun param body) (closure param body ds)]
    [(app fun args) (let ([fun-val (interp fun ds)])
                      (interp (closure-body fun-val)
                              (aSub (first (closure-param fun-val))
                                    (first (interp-env args ds))
                                    (closure-env fun-val))))]))



(define (toma-args lst)
  (match lst
    ['() '()]
    [(cons x xs) 3]))

;; Concatena dos listas del tipo lisT
;; CFWBAE CFWBAE --> CFWBAE
(define (appT l1 l2)
  (match l1
    ['() l2]
    [(lisT '()) (l2)]
    [(lisT xs) (match l2
                 [(lisT '()) l1]
                 [(lisT ys) (lisT (appL xs ys))])]))

;; Concatena dos listas
;; appL :: CFWBAE CFWBAE --> CFWBAE
(define (appL l1 l2)
  (match l1
    ['() l2]
    [(cons x xs) (match l2
                   ['() l1]
                   [ys (cons x (appL xs ys))])]))

;; Calcula la longitud de una lista de tipo lisT
;; lengthT :: CFWBAE --> number
(define (lengthT sexp)
  (match sexp
    [(lisT '()) 0]
    [(lisT (cons x xs)) (+ 1 (lengthT (lisT xs)))]))

;; Interpreta todos los elementos de una lista de CFWBAE
;; interp-env :: (listof CFWBAE) DefrdSub --> (listof CFWBAE-Value)
(define (interp-env zs ds)
  (match zs
    ['() '()]
    [(cons x xs) (cons (interp x ds) (interp-env xs ds))]))

;; Busca en el ambiente un número y lo devuelve
;; get-num :: CFWBAE DefrdSub --> CFWBAE
(define (get-num sexp ds)
  (match sexp
    [(num n) sexp]
    [(id x) (cond
              [(number? (get-param (lookup x (repeat-id ds)))) (num (numV-n (lookup x (repeat-id ds))))]
              [else (error 'interp "La operación sólo acepta números como parámetro")])]))

;; Busca en el ambiente un booleano y lo devuelve
;; get-bool :: CFWBAE DefrdSub --> CFWBAE
(define (get-bool sexp ds)
  (match sexp
    [(bool n) sexp]
    [(id x) (cond
              [(bool? (get-param (lookup x (repeat-id ds)))) (bool (boolV-b (lookup x (repeat-id ds))))]
              [else (error 'interp "La operación sólo acepta booleanos como parámetro")])]))

;; Busca en el ambiente un caracter y lo devuelve
;; get-char :: CFWBAE DefrdSub --> (CFWBAE)
(define (get-char sexp ds)
  (match sexp
    [(chaR n) sexp]
    [(id x) (chaR (charV-c (lookup x (repeat-id ds))))]))

;; Busca en el ambiente una cadena y lo devuelve
;; get-str :: CFWBAE DefrdSub --> (CFWBAE)
(define (get-str sexp ds)
  (match sexp
    [(strinG n) sexp]
    [(id x) (cond
              [(string? (get-param (lookup x (repeat-id ds)))) (strinG (stringV-s (lookup x (repeat-id ds))))]
              [else (error 'interp "La operación sólo acepta cadenas como parámetro")])]))

;; Busca en el ambiente una lista y lo devuelve
;; get-lts :: CFWBAE DefrdSub --> (CFWBAE)
(define (get-lts sexp ds)
  (match sexp
    [(lisT n) sexp]
    [(id x) (cond
              [(list? (get-param (lookup x (repeat-id ds)))) (lisT (map (lambda (x) (to-cfwbae (get-param x))) (listV-l (lookup x (repeat-id ds)))))]
              [else (error 'interp "La operación sólo acepta listas como parámetro")])]))

;; Tranforma un elemento a uno de tipo CFWBAE
;; cf :: any --> CFWBAE
(define (to-cfwbae exp)
  (cond
    [(number? exp) (num exp)]
    [(boolean? exp) (bool exp)]
    [(char? exp) (chaR exp)]
    [(string? exp) (strinG exp)]
    [(list? exp) (lisT (map (lambda (x) (to-cfwbae x)) exp))]))

;; Transforma un elemento a uno de tipo CFWBAE-Value
;; cv :: any --> CFWBAE-Value
(define (cv exp)
  (cond
    [(number? exp) (numV exp)]
    [(boolean? exp) (boolV exp)]
    [(char? exp) (charV exp)]
    [(string? exp) (stringV exp)]
    [(list? exp) (listV (map (lambda (x) (cv x)) exp))]))

;; Devuelve el parámetro de cada tipo de dato
;; get-param :: CFWBAE-Value --> any
(define (get-param expr)
  (match expr
    [(numV n) n]
    [(boolV b) b]
    [(charV c) c]
    [(stringV s) s]
    [(listV l) l]))

;; Verifica si hay identificadores repetidos en un caché de sustituciones
;; id-repeat :: DfrSub --> boolean
(define (id-repeat ds)
  (repeat (extract-ids ds)))

;; Extrae los identificadores de un cache de susticiones
;; extract-ids :: DfrSub --> (listof symbol)
(define (extract-ids ds)
  (match ds
    [(mtSub) '()]
    [(aSub id value env) (cons id (extract-ids env))]))

;; Verifica sii hay elementos repetidos en una lista
;; repeat :: (listof any) --> boolean
(define (repeat lst)
  (match lst
    ['() #f]
    [(cons x xs) (pertenece? x xs)]))

;; Predicado que verifica si un elemnto se encuentra en una lista
;; pertenece? :: any (listof any) --> boolean
(define (pertenece? e lst)
  (cond
    [(equal? (length lst) 0) #f]
    [(equal? e (car lst)) #t]
    [else (pertenece? e (cdr lst))]))

;; Verifica si hay identificadores repetidos eb el caché de sustituciones
;; repeat-id :: DfrSub --> boolean
(define (repeat-id ds)
  (cond
    [(equal? (id-repeat ds) #f) ds]
    [else (error 'interp "Hay identificadores repetidos en el caché de sustituciones")]))

;(require racket/trace)
;(trace interp)
;(trace interp-env)
;(interp (desugar (parse '{with [(f (fun (x) (+ x 2)))] {f (n)}})) (aSub 'n (numV 3) (mtSub)))
;(interp (desugar (parse '{+ 4 x})) (aSub 'x (numV 4) (mtSub)))
;(interp (op cons (list (num 3) (lisT (list (bool #f) (chaR #\c))))) (mtSub))
;(interp (desugar (parse '{with [(f (fun (x) (+ x x))) (xs (lst 2 3))] {cons 2 (lst 4 5)}})) (mtSub))
;(interp-env (list (num 4) (num 3)) (mtSub))
;(interp (desugar (parse '{(fun (x y) (+ x y)) (3 4)})) (mtSub))