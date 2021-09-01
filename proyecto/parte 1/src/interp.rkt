#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup :: symbol DefrdSub --> CFWBAE
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
                         [else (interp (iF (bool (cond
                                                   [(boolV? (interp cnd ds)) (boolV-b (interp cnd ds))]
                                                   [else (error 'interp "La condicional de if no es un booleano")]))
                                           then els) ds)])]
    [(op f lst) (let* ([a (map (lambda (x) (cond
                                             [(numV? (interp x ds)) (numV-n (interp x ds))]
                                             [(boolV? (interp x ds)) (boolV-b (interp x ds))]
                                             [(charV? (interp x ds)) (charV-c (interp x ds))]
                                             [(stringV? (interp x ds)) (stringV-s (interp x ds))]
                                             [(listV? (interp x ds))
                                              (map (lambda (y) (get-param y)) (listV-l (interp x ds)))])) lst)]
                                [b (verified-args f a)])
                              (cond
                                [(number? b) (numV b)]
                                [(boolean? b) (boolV b)]
                                [(char? b) (charV b)]
                                [(string? b) (stringV b)]
                                [(list? b) (listV (map (lambda (z) (interp (to-cfwbae z) ds)) b))]))]
    [(fun param body) (closure param body ds)]
    [(app fun args) (let ([fun-val (interp fun ds)])
                      (interp (closure-body fun-val)
                               (interp-app (closure-param fun-val) args (closure-env fun-val) ds)))]))

;; Tranforma un elemento a uno de tipo CFWBAE
;; cf :: any --> CFWBAE
(define (to-cfwbae exp)
  (cond
    [(number? exp) (num exp)]
    [(boolean? exp) (bool exp)]
    [(char? exp) (chaR exp)]
    [(string? exp) (strinG exp)]
    [(list? exp) (lisT (map (lambda (x) (to-cfwbae x)) exp))]))

;; Devuelve el parámetro de cada tipo de dato
;; get-param :: CFWBAE-Value --> any
(define (get-param expr)
  (match expr
    [(numV n) n]
    [(boolV b) b]
    [(charV c) c]
    [(stringV s) s]
    [(listV l) l]))


;; Verifica si los operadores poseen los tipos correctos
;; verified-args :: procedure (listof any) --> any
(define (verified-args f args)
  (cond 
    [(or (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f modulo) (equal? f expt)
         (equal? f <) (equal? f <=) (equal? f =) (equal? f >=) (equal? f >) (equal? f zero?)
         (equal? f add1) (equal? f sub1)) (cond
                                            [(andmap number? args)  (apply f args)]
                                            [else (error 'interp "El operador sólo acepta números")])]
    [(or (equal? f anD) (equal? f oR)
         (equal? f not)) (cond
                           [(andmap boolean? args) (apply f args)]
                           [else (error 'interp "El operador sólo acepta booleanos")])]
    [(or (equal? f string-append)
         (equal? f string-length)) (cond
                                     [(andmap string? args) (apply f args)]
                                     [else (error 'interp "El operador sólo acepta cadenas")])]
    [(or (equal? f append) (equal? f length)
         (equal? f car) (equal? f cdr)) (cond
                                          [(andmap list? args) (apply f args)]
                                          [else (error 'interp "El operador sólo acepta listas")])]
    [(equal? f cons) (cond
                       [(list? (cdr args)) (apply f args)]
                       [else (error 'interp "El operador sólo acepta un elemento y una lista")])]
    [else (apply f args)]))

;; Función que acarrea los id y valores en el ambiente
;; interp-app :: (listof symbol) (listof CFWBAE-Value) DefrdSub DefrdSub --> DefrdSub 
(define (interp-app param arg env ds)
  (cond
    [(and (empty? param) (empty? arg)) env]
    [(equal? (length param) (length arg))
     (interp-app (cdr param) (cdr arg) (aSub (first param) (interp (first arg) ds) env) ds)]
    [else (error 'interp "La cantidad de parámetros y agumentos debe ser la misma")]))

;;(require racket/trace)
;;(trace interp)



