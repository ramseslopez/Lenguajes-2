#lang plai

(require (file "./grammars.rkt"))
;(require (file "./parser.rkt"))

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
                                    [(cons (binding a b) zs) (subst (with zs body) (aSub a b ds))])] ))
        ;[(with* ys body) ()]))

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
                                          [else "La longitud de la lista es mayor a 1"])]
                              [add1 (cond
                                          [(equal? (length zs) 1) (num (add1 (first zs)))]
                                          [else "La longitud de la lista es mayor a 1"])]
                              [modulo (cond
                                              [(equal? (length zs) 2) (num (modulo (first zs) (last zs)))]
                                              [else (error "La longitud de la lista es mayor a 2")])]
                              [expt (cond
                                              [(equal? (length zs) 2) (num (expt (first zs) (last zs)))]
                                              [else (error "La longitud de la lista es mayor a 2")])])]))


(define (bind-conv x)
    (match x
      [(binding a b) (list a b)]))

(define (bind-list xs)
    (map (lambda (x) (bind-conv x)) xs))


(define (subst-aux expr sub-id value)
    (match expr
        [(id i) (cond
                      [(equal? i sub-id) value]
                      [else expr])]
        [(num n) expr]
        [(op f lst) (op f (map (lambda (x) (subst-aux x sub-id value)) lst))]))

#|(define (sub-bind lst)
    (match lst
        ['() lst]
        [(cons x xs) (cond
                                [(pertenece? (second (bind-conv x)) (snds xs)) (subst-aux (second (bind-conv x)) () (first ()))]
                                [else (sub-bind xs)])]))|#

;; Predicado que verifica si un elemnto se encuentra en una lista
;; pertenece? :: any (listof any) --> boolean
(define (pertenece? e lst)
  (cond
    ;;[(null? e) (error "No hay elemento por buscar")]
    [(equal? (length lst) 0) #f]
    [(equal? e (car lst)) #t]
    [else (pertenece? e (cdr lst))]))

(define (snds xs)
    (match xs
        ['() '()]
        [(cons (binding a b) ys) (cons b (snds ys))]))

(define (out sym lst)
    (match lst
        ['() lst]
        [(cons (binding a b) xs) (cond
                                                [(equal? sym a) (a b)]
                                                [else (out sym xs)])]))
