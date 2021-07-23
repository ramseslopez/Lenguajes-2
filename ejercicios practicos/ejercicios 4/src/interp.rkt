#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> WAE
(define (lookup name ds)
    (match ds
        [(mtSub) (error "Variable libre")]
        [(aSub id value env) (cond
                                           [(equal? id name) value]
                                           [else (lookup name env)])]))

;; Toma un árbol de sintáxis abstraca del lenguaje WAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: WAE DefrdSub -> WAE
(define (interp expr ds)
  ...)
