#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup :: symbol DefrdSub -> WAE
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
    (operate (subst expr ds)))

;; Realiza la sustitución de las variables almacenadas en el ambiente
;; subst :: WAE DefrdSub --> WAE
(define (subst expr ds)
      (match expr
        [(id i) (lookup i ds)]
        [(num n) expr]
        [(op f xs) (op f (map (lambda (x) (subst x ds)) xs))]
        [(with ys body) (match ys
                                    ['() (subst body ds)]
                                    [(cons (binding a b) zs) (subst (with zs body) (aSub a b ds))])]))

;; Realiza la operación correspondiente acorde con cada expresión
;; operate :: WAE --> WAE
(define (operate xs)
        (match xs
          [(id i) xs]
          [(num n) xs]
          [(op g zs) (match g
                              [+ (num (apply + (map num-n zs)))]
                              [- (num (apply - (map num-n zs)))]
                              [* (num (apply * (map num-n zs)))]
                              [/ (num (apply / (map num-n zs)))]
                              [sub1 (cond
                                          [(equal? (length zs) 1) (num (sub1 (first zs)))]
                                          [else (num 1)])]
                              [add1 (cond
                                          [(equal? (length zs) 1) (num (add1 (first zs)))]
                                          [else "La cardinalidad de la lista es mayor a 1"])]
                              [modulo (cond
                                              [(equal? (length zs) 2) (num (modulo (first zs) (last zs)))]
                                              [else (error "La cardinalidad de la lista es mayor a 2")])]
                              [expt (cond
                                              [(equal? (length zs) 2) (num (expt (first zs) (last zs)))]
                                              [else (error "La cardinalidad de la lista es mayor a 2")])])]))
