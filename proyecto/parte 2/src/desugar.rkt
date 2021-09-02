#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo RCFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar :: SRCFWBAE --> RCFWBAE
(define (desugar sexpr)
  (match sexpr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(charS c) (chaR c)]
    [(stringS s) (strinG s)]
    [(listS lst) (lisT (map (lambda (x) (desugar x)) lst))]
    [(iFS c t e) (iF (desugar c) (desugar t) (desugar e))]
    [(iF0 n t e) (iF (op zero? (list (desugar n))) (desugar t) (desugar e))]
    [(opS f args) (op f (map (lambda (x) (desugar x)) args))]
    [(condS lst) (ifos sexpr)]
    [(withS lst body) (app (fun (fsts lst) (desugar body)) (map (lambda (x) (desugar x)) (snds lst)))]
    [(withS* lst body) (desugar (deswith sexpr))]
    [(recS lst body) (rec (rec-aux lst) (desugar body))]
    [(funS param type body) (fun param (desugar body))]
    [(appS fun args) (app (desugar fun) (map (lambda (x) (desugar x)) args))]))

;; Obtiene el primer elemento de un Binding
;; fsts :: (listof Binding) --> (listof symbol)
(define (fsts xs)
  (match xs
    ['() '()]
    [(cons (bindingS a t b) ys) (cons (param a t) (fsts ys))]))

;; Obtiene el segundo elemento de un Binding
;; snds :: (listof Binding) --> (listof SCFWBAE)
(define (snds xs)
  (match xs
    ['() '()]
    [(cons (bindingS a t b) ys) (cons b (snds ys))]))

;; Transforma un withS* a una serie de withS anidados
;; deswith :: SRCFWBAE --> SRCFWBAE
(define (deswith sexpr)
  (match sexpr
    [(withS* ys body) (cond
                        [(equal? (length ys) 1) (withS ys body)]
                        [else (withS (list (car ys)) (deswith (withS* (cdr ys) body)))])]))

;; Transforma una lista de condicionales a una serie de if's anidados
;; ifos :: SRCFWBAE --> RCFWAE
(define (ifos sexpr)
  (match sexpr
    [(condS lst) (cond
                   [(equal? (length lst) 2) (iF (desugar (first (if1 (first lst))))
                                                (desugar (second (if1 (first lst))))
                                                (desugar (first (if2 (second lst)))))]
                   [else (iF (desugar (first (if1 (first lst))))
                             (desugar (second (if1 (first lst))))
                             (ifos (condS (cdr lst))))])]))

;; Transforma un condition a una lista
;; if1 :: Condition --> (listof SRCFWBAE)
(define (if1 cnd)
  (match cnd
    [(condition a b) (list a b)]))

;; Transforma un condition a una lista
;; if2 :: Condition --> (listof SRCFWBAE)
(define (if2 cnd)
  (match cnd
    [(else-cond a) (list a)]))

;; Función auxiliar que quita el azúcar a una lista de bindings
;; rec-aux :: (listof BindingS) --> (listof Binding)
(define (rec-aux bindings)
  (match bindings
    ['() '()]
    [(cons (bindingS a t b) xs) (cons (binding a (desugar b)) (rec-aux xs))]))

