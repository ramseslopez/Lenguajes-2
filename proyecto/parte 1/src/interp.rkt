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
                                  [else (error 'interp "La relación < sólo acepta números como parámetros")])]
                  [(equal? f <=) (match lst
                                   [(cons (num x) (cons (num y) '())) (boolV (<= x y))]
                                   [else (error 'interp "La relación <= sólo acepta números como parámetros")])]
                  [(equal? f >=) (match lst
                                   [(cons (num x) (cons (num y) '())) (boolV (>= x y))]
                                   [else (error 'interp "La relación >= sólo acepta números como parámetros")])]
                  [(equal? f >) (match lst
                                  [(cons (num x) (cons (num y) '())) (boolV (> x y))]
                                  [else (error 'interp "La relación > sólo acepta números como parámetros")])]
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
                                     [else (error 'interp "La operación sucesor sólo acepta un número como parámetro")])]
                  [(equal? f sub1) (match lst
                                     [(cons (num x) '()) (boolV (sub1 x))]
                                     [else (error 'interp "La operación predecesor sólo acepta un número como parámetro")])]
                  [(equal? f oR) (match lst
                                   [(cons (bool x) (cons (bool y) '())) (boolV (or x y))]
                                   [else (error 'interp "La operación or sólo acepta booleanos como parámetros")])]
                  [(equal? f anD) (match lst
                                    [(cons (bool x) (cons (bool y) '())) (boolV (and x y))]
                                    [else (error 'interp "La operación and sólo acepta booleanos como parámetros")])]
                  [(equal? f not) (match lst
                                    [(cons (bool x) '()) (boolV (not x))]
                                    [else (error 'interp "La operación not sólo acepta un booleano como parámetro")])]
                  [(equal? f zero?) (match lst
                                      [(cons (num x) '()) (boolV (zero? x))]
                                      [else (error 'interp "La operación zero? sólo acepta un número como parámetro")])]
                  [(equal? f num?) (match lst
                                     [(cons (num x) '()) (boolV (num? x))]
                                     [else (error 'interp "La operación nu? sólo acepta un número como parámetro")])]
                  [(equal? f bool?) (match lst
                                      [(cons (bool x) '()) (boolV (bool? x))]
                                      [else (error 'interp "La operación bool? sólo acepta un booleano como parámetro")])]
                  [(equal? f char?) (match lst
                                      [(cons (chaR x) '()) (boolV (char? x))]
                                      [else (error 'interp "La operación char? sólo acepta un caracter como parámetro")])]
                  [(equal? f string?) (match lst
                                        [(cons (strinG x) '()) (boolV (string? x))]
                                        [else (error 'interp "La operación string? sólo acepta una cadena como parámetro")])]
                  [(equal? f list?) (match lst
                                      [(cons (lisT x) '()) (boolV (list? x))]
                                      [else (error 'interp "La operación list? sólo acepta una lista como parámetro")])]
                  [(equal? f empty?) (match lst
                                       [(cons (lisT x) '()) (boolV (empty? x))]
                                       [else (error 'interp "La operación empty? sólo acepta una lista como parámetro")])]
                  [(equal? f cons) (match lst
                                     [(cons x (list (lisT xs))) (interp (lisT (cons x xs)) ds)]
                                     [else (error 'interp "La operación cons sólo acepta una elemento y una lista como parámetros")])]
                  [(equal? f car) (match lst
                                    [(list (lisT (cons x xs))) (interp x ds)]
                                    [else (error 'interp "La operación car sólo acepta una lista como parámetro")])]
                  [(equal? f cdr) (match lst
                                    [(list (lisT (cons x xs))) (interp (lisT xs) ds)]
                                    [else (error 'interp "La operación cdr sólo acepta una lista como parámetro")])]
                  [(equal? f append) (match lst
                                       [(cons x '()) (interp x ds)]
                                       [(cons (lisT x) (cons (lisT y) ys))  (interp (op append (cons (appT (lisT x) (lisT y)) ys)) ds)]
                                       [else (error 'interp "La operación append sólo acepta listas como parámetros")])]
                  [(equal? f length) (match lst
                                       ['() (numV 0)]
                                       [(list '()) (numV 0)]
                                       [(list (lisT '())) (numV 0)]
                                       [(cons x xs) (numV (lengthT (lisT lst)))])]
                  )]))


(define (vtoint n)
  (match n
    [(numV a) a]))

;; CFWAE CFWAE --> CFWAE
(define (appT l1 l2)
  (match l1
    ['() l2]
    [(lisT '()) (l2)]
    [(lisT xs) (match l2
                 [(lisT '()) l1]
                 [(lisT ys) (lisT (appL xs ys))])]))

;; appL :: CFWAE CFWAE --> CFWAE
(define (appL l1 l2)
  (match l1
    ['() l2]
    [(cons x xs) (match l2
                   ['() l1]
                   [ys (cons x (appL xs ys))])]))

(define (lengthT sexp)
  (match sexp
    [(lisT '()) 0]
    [(lisT (cons x xs)) (+ 1 (lengthT (lisT xs)))]))


;(interp (desugar (parse '{length {1 2 3 4 5}})) (mtSub))

;(interp (op + '()) (mtSub))
;(interp (iF (bool #f) (num 6) (num 8)) (mtSub))
