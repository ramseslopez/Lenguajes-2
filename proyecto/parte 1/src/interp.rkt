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
				 [(op f lst) (match f
														[not (match lst
																				[(cons (bool x) '()) (boolV (not x))]
																				[else (error 'interp "ERROR not")])]
														[< (match lst
																			[(cons (num x) (cons (num y) '())) (boolV (< x y))]
																			[else (error 'interp "ERROR <")])]

														[+ (match lst
																			['() (numV 0)]
																			[(cons (num x) xs) (numV (apply + (map num-n (cons (num x) xs))))]
																			[else (error 'interp "La suma sólo opera con números")])]
														[- (match lst
																			['() (numV 0)]
																			[(cons (num x) xs) (numV (apply - (map num-n (cons (num x) xs))))]
																			[else (error 'interp "La resta sólo opera con números")])]
														[* (match lst
																			['() (numV 0)]
																			[(cons (num x) xs) (numV (apply * (map num-n (cons (num x) xs))))]
																			[else (error 'interp "La *sólo opera con números")])]

														[oR (match lst
																			 [(cons (bool x) (cons (bool y) '())) (boolV (or x y))]
																			 [else (error 'interp "ERROR or")])]
														[zero? (match lst
																					[(cons (num x) '()) (boolV (zero? x))]
																					[else (error 'interp "ERROR zero?")])]
														)]))


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
