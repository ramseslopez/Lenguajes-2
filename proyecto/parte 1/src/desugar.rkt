#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar :: SCFWBAE --> CFWBAE
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
    [(funS lst body) (fun lst (desugar body))]
    [(appS f args) (app (desugar f) (map (lambda (x) (desugar x)) args))]))


;; Obtiene el primer elemento de un Binding
;; fsts :: (listof Binding) --> (listof symbol)
(define (fsts xs)
  (match xs
    ['() '()]
    [(cons (binding a b) ys) (cons a (fsts ys))]))

;; Obtiene el segundo elemento de un Binding
;; snds :: (listof Binding) --> (listof SCFWBAE)
(define (snds xs)
  (match xs
    ['() '()]
    [(cons (binding a b) ys) (cons b (snds ys))]))

;; Transforma un withS* a una serie de withS anidados
;; deswith :: SCFWBAE --> SCFWBAE
(define (deswith sexpr)
  (match sexpr
    [(withS* ys body) (cond
                        [(equal? (length ys) 1) (withS ys body)]
                        [else (withS (list (car ys)) (deswith (withS* (cdr ys) body)))])]))

;; Transforma una lista de condicionales a una serie de if's anidados
;; ifos :: SCFWBAE --> CFWAE
(define (ifos sexpr)
  (match sexpr
    [(condS lst) (cond
                   [(equal? (length lst) 2) (iF (desugar (first (if1 (first lst)))) (desugar (second (if1 (first lst)))) (desugar (first (if2 (second lst)))))]
                   [else (iF (desugar (first (if1 (first lst)))) (desugar (second (if1 (first lst)))) (ifos (condS (cdr lst))))])]))

;; Transforma un condition a una lista
;; if1 :: Condition --> (listof SCFWBAE)
(define (if1 cnd)
  (match cnd
    [(condition a b) (list a b)]))

;; Transforma un condition a una lista
;; if1 :: Condition --> (listof SCFWBAE)
(define (if2 cnd)
  (match cnd
    [(else-cond a) (list a)]))
