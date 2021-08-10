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
				 [(strinG str) (charV str)]
				 [(lisT xs) (listV (map (lambda (x) (interp x ds)) xs))]
				 [(iF cnd then els) (cond
																					[(equal? cnd (bool #t)) (interp then ds)]
																					[(equal? cnd (bool #f)) (interp els ds)]
																					[else (iF (interp cnd ds) then els)])]
				 [(op f lst) (cond
															 [(equal? f <) (match lst
																													[(cons (num x) (cons (num y) '())) (boolV (< x y))]
																													[else (error 'interp "ERROR <")])]
																	[(equal? f <=) (match lst
																															[(cons (num x) (cons (num y) '())) (boolV (<= x y))]
																															[else (error 'interp "ERROR <=")])]
																[(equal? f >=) (match lst
																														[(cons (num x) (cons (num y) '())) (boolV (>= x y))]
																														[else (error 'interp "ERROR >=")])]
																[(equal? f >) (match lst
																													[(cons (num x) (cons (num y) '())) (boolV (> x y))]
																													[else (error 'interp "ERROR >")])]
																[(equal? f +) (match lst
																														['() (numV 0)]
																														[(cons (num x) xs) (numV (apply + (map num-n (cons (num x) xs))))]
																														[else (error 'interp "La suma sólo opera con números")])]
															  [(equal? f -) (match lst
																										['() (numV 0)]
																										[(cons (num x) xs) (numV (apply - (map num-n (cons (num x) xs))))]
																										[else (error 'interp "La resta sólo opera con números")])]
															 [(equal? f *) (match lst
																										['() (numV 0)]
																										[(cons (num x) xs) (numV (apply * (map num-n (cons (num x) xs))))]
																										[else (error 'interp "La multiplicación sólo opera con números")])]
																[(equal? f /) (match lst
																										['() (numV 0)]
																										[(cons (num x) xs) (numV (apply / (map num-n (cons (num x) xs))))]
																										[else (error 'interp "La división sólo opera con números")])]
															 [(equal? f add1) (match lst
																											[(cons (num x) '()) (boolV (add1 x))]
																											[else (error 'interp "ERROR add1")])]
															 [(equal? f sub1) (match lst
																											[(cons (num x) '()) (boolV (sub1 x))]
																											[else (error 'interp "ERROR sub1")])]
															 [(equal? f oR) (match lst
																										 [(cons (bool x) (cons (bool y) '())) (boolV (or x y))]
																										 [else (error 'interp "ERROR or")])]
																[(equal? f anD) (match lst
																										 [(cons (bool x) (cons (bool y) '())) (boolV (and x y))]
																										 [else (error 'interp "ERROR or")])]
															 [(equal? f not) (match lst
																											[(cons (bool x) '()) (boolV (not x))]
																											[else (error 'interp "ERROR not")])]
															 [(equal? f zero?) (match lst
																												[(cons (num x) '()) (boolV (zero? x))]
																												[else (error 'interp "ERROR zero?")])]
																[(equal? f num?) (match lst
																												[(cons (num x) '()) (boolV (num? x))]
																												[else (error 'interp "ERROR num?")])]
																[(equal? f bool?) (match lst
																												[(cons (bool x) '()) (boolV (bool? x))]
																												[else (error 'interp "ERROR bool?")])]
																[(equal? f char?) (match lst
																												[(cons (chaR x) '()) (boolV (char? x))]
																												[else (error 'interp "ERROR char?")])]
																[(equal? f string?) (match lst
																												[(cons (strinG x) '()) (boolV (string? x))]
																												[else (error 'interp "ERROR string	?")])]
																[(equal? f list?) (match lst
																												[(cons (lisT x) '()) (boolV (list? x))]
																												[else (error 'interp "ERROR list	?")])]
																[(equal? f empty?) (match lst
																												[(cons (lisT x) '()) (boolV (empty? x))]
																												[else (error 'interp "ERROR empty	?")])]
																	[(equal? f cons) (match lst
																											 	[(cons x (list (lisT xs))) (interp (lisT (cons x xs)) ds)]
																											 	[else (error 'interp "ERROR cons")])]
																	[(equal? f car) (match lst
																														[(list (lisT (cons x xs))) (interp x ds)]
																											 			[else (error 'interp "ERROR car")])]
																	[(equal? f cdr) (match lst
																														[(list (lisT (cons x xs))) (interp (lisT xs) ds)]
																											 			[else (error 'interp "ERROR cdr")])]
																	[(equal? f append) (match lst
																												[(cons x '()) (interp x ds)]
																											 	[(cons (lisT x) (list xs)) (interp (lisT (append x  (interp (op append xs) ds))) ds)]
																											 	[else (error 'interp "ERROR append")])]
															 )]))


(define (concatena l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (concatena (cdr l1) l2))]))
;(interp (op + '()) (mtSub))
(interp (iF (bool #f) (num 6) (num 8)) (mtSub))


#|(define (interp expr ds)
		(operate (subst expr ds)))

;; Realiza la sustitución de las variables almacenadas en el ambiente
;; subst :: CFWBAE DefrdSub --> CFWBAE
(define (subst expr ds)
	(match expr
				 [(id i) (lookup i ds)]
				 [(num n) (numV n)]
				 [(op f xs) (op f (map (lambda (x) (subst x ds)) xs))]))

;; Realiza la operación correspondiente acorde con cada expresión
;; operate :: CFWBAE --> CFWBAE-Value
(define (operate lst)
	(match lst
				 [(id i) (error 'interp "Identificador libre")]
				 [(num n) (numV n)]
				 [(bool b) (boolV b)]
				 [(chaR chr) (charV chr)]
				 [(strinG str) (charV str)]
				 [(lisT xs) (listV (map (lambda (x) (operate x)) xs))]
				 [(iF cond then else) (cond
																[(equal? cond (bool #t)) (operate then)]
																[(equal? cond (bool #f)) (operate else)]
																[else (iF (operate cond) then else)])]))

(define (desnum numn)
	(match numn
				 [(num n) numn]
				 [(numV m) (num m)]))|#
