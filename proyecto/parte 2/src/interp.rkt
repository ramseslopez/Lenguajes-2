#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;;Función para obtener una nueva localización para la siguiente caja.
;;No recibe parámetros
;; new-location: -> Number
(define new-location
  (let ([n (box 0)])
    (λ ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))
    

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol Env -> RCFWBAE-Typed
;; (define (lookup name ds)
(define (lookup name ds)
 )

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: RCFWBAE-Typed DefrdSub-> RCFWBAE-Value
(define (interp expr ds)
  ...)

