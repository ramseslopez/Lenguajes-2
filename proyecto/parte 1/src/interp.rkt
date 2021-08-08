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
        [(mtSub) (error "Variable libre")]
        [(aSub id value env) (cond
                                           [(equal? id name) value]
                                           [else (lookup name env)])]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp :: CFWBAE DefrdSub --> CFWBAE-Value
(define (interp expr ds)
    (operate (subst expr ds)))

;; Realiza la sustitución de las variables almacenadas en el ambiente
;; subst :: CFWBAE DefrdSub --> CFWBAE
(define (subst expr ds)
    (match expr
      [(id i) (lookup i ds)]
      [(num n) expr]
      [(op f xs) (op f (map (lambda (x) (subst x ds)) xs))]))

;; Realiza la operación correspondiente acorde con cada expresión
;; operate :: CFWBAE --> CFWBAE-Value
(define (operate xs)
    (match xs
        [(id i) xs]
        [(num n) (numV n)]
        [(op g zs) (match g
                            #|[+ (numV (apply + (map num-n (map desnum zs))))]
                            [- (numV (apply - (map num-n (map desnum zs))))]
                            [* (numV (apply * (map num-n (map desnum zs))))]
                            [/ (numV (apply / (map num-n (map desnum zs))))]|#
                            [sub1 (cond
                                        [(equal? (length zs) 1) (numV (sub1 (first zs)))]
                                        [else "La cardinalidad de la lista es mayor a 1"])]
                            [add1 (cond
                                        [(equal? (length zs) 1) (numV (add1 (first zs)))]
                                        [else "La cardinalidad de la lista es mayor a 1"])]
                            [modulo (cond
                                            [(equal? (length zs) 2) (numV (modulo (first zs) (last zs)))]
                                            [else (error "La cardinalidad de la lista es mayor a 2")])]
                            [expt (cond
                                            [(equal? (length zs) 2) (numV (expt (first zs) (last zs)))]
                                            [else (error "La cardinalidad de la lista es mayor a 2")])])]))

(define (desnum numn)
    (match numn
        [(num n) numn]
        [(numV m) (num m)]))

(define (oR p q)
    (or p q))

(define (anD p q)
    (and p q))
