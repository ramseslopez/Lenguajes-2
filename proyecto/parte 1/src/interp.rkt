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
        [(mtSub) (error 'lookup "Variable libre")]
        [(aSub id value env) (cond
                                           [(equal? id name) value]
                                           [else (lookup name env)])]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp :: CFWBAE DefrdSub --> CFWBAE-Value
;(define (interp expr ds)    (operate (subst expr ds)))

;; Realiza la sustitución de las variables almacenadas en el ambiente
;; subst :: CFWBAE DefrdSub --> CFWBAE
(define (subst expr ds)
    (match expr
      [(id i) (lookup i ds)]
      [(num n) expr]
      [(op f xs) (op f (map (lambda (x) (subst x ds)) xs))]))

;; Realiza la operación correspondiente acorde con cada expresión
;; operate :: CFWBAE --> CFWBAE-Value
;(define (operate xs)    ())

(define (desnum numn)
    (match numn
        [(num n) numn]
        [(numV m) (num m)]))
