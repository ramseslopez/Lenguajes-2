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
    [(strinG str) (stringV str)]
    [(lisT xs) (listV (map (lambda (x) (interp x ds)) xs))]
    [(iF cnd then els) (cond
                         [(equal? cnd (bool #t)) (interp then ds)]
                         [(equal? cnd (bool #f)) (interp els ds)]
                         [else (iF (interp cnd ds) then els)])]
    [(op f lst) (cond
                  [(equal? f <) (match lst
                                  [(cons (id i) xs) (boolV (< (cond
                                                                [(number? (intV (lookup i ds))) (numV-n (lookup i ds))]
                                                                [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                              (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (boolV (< x (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) (cons (num y) '())) (boolV (< x y))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f <=) (match lst
                                   [(cons (id i) xs) (boolV (<= (cond
                                                                  [(number? (intV (lookup i ds))) (numV-n (lookup i ds))]
                                                                  [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                                (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                   [(cons (num x) xs) (boolV (<= x (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                   [(cons (num x) (cons (num y) '())) (boolV (<= x y))]
                                   [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f >=) (match lst
                                   [(cons (id i) xs) (boolV (>= (cond
                                                                  [(number? (intV (lookup i ds))) (numV-n (lookup i ds))]
                                                                  [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                                (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                   [(cons (num x) xs) (boolV (>= x (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                   [(cons (num x) (cons (num y) '())) (boolV (>= x y))]
                                   [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f >) (match lst
                                  [(cons (id i) xs) (boolV (> (cond
                                                                [(number? (intV (lookup i ds))) (numV-n (lookup i ds))]
                                                                [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                              (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (boolV (> x (num-n (first (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) (cons (num y) '())) (boolV (> x y))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f +) (match lst
                                  ['() (numV 0)]
                                  [(cons (id i) xs) (numV (apply + (map num-n (cons (cond
                                                                                      [(number? (intV (lookup i ds))) (num (numV-n (lookup i ds)))]
                                                                                      [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                                                    (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (numV (apply + (map num-n (cons (num x) (map (lambda (c) (int-id c ds)) xs)))))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f -) (match lst
                                  ['() (numV 0)]
                                  [(cons (id i) xs) (numV (apply - (map num-n (cons (cond
                                                                                      [(number? (intV (lookup i ds))) ((num (numV-n (lookup i ds))))]
                                                                                      [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                                                    (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (numV (apply - (map num-n (cons (num x) (map (lambda (c) (int-id c ds)) xs)))))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f *) (match lst
                                  ['() (numV 0)]
                                  [(cons (id i) xs) (numV (apply * (map num-n (cons (cond
                                                                                      [(number? (intV (lookup i ds))) (num (numV-n (lookup i ds)))]
                                                                                      [else (error 'interp "La operación sólo acepta números como parámetro")])
                                                                                    (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (numV (apply * (map num-n (cons (num x) (map (lambda (c) (int-id c ds)) xs)))))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f /) (match lst
                                  ['() (numV 0)]
                                  [(cons (id i) xs) (numV (apply / (map num-n (cons (cond
                                                                                      [(number? (intV (lookup i ds))) (num (numV-n (lookup i ds)))]
                                                                                      [else (error 'interp "La operación sólo opera con números")]) (map (lambda (c) (int-id c ds)) xs)))))]
                                  [(cons (num x) xs) (numV (apply / (map num-n (cons (num x) (map (lambda (c) (int-id c ds)) xs)))))]
                                  [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f add1) (match lst
                                     [(cons (id i) '()) (cond
                                                          [(number? (intV (lookup i ds))) (numV (add1 (numV-n (lookup i ds))))]
                                                          [else (error 'interp "La operación sólo acepta números como parámetro")])]
                                     [(cons (num x) '()) (numV (add1 x))]
                                     [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f sub1) (match lst
                                     [(cons (id i) '()) (cond
                                                          [(number? (intV (lookup i ds))) (numV (sub1 (numV-n (lookup i ds))))]
                                                          [else (error 'interp "La operación sólo acepta números como parámetro")])]
                                     [(cons (num x) '()) (numV (sub1 x))]
                                     [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f oR) (match lst
                                   [(cons (id i) xs) (boolV (or (cond
                                                                  [(boolean? (intV (lookup i ds))) (boolV-b (lookup i ds))]
                                                                  [else (error 'interp "La operación sólo acepta booleanos como parámetro")])
                                                                (bool-b (first (map (lambda (c) (int-id-bool c ds)) xs)))))]
                                   [(cons (bool x) xs) (boolV (or x (bool-b (first (map (lambda (c) (int-id-bool c ds)) xs)))))]
                                   [(cons (bool x) (cons (bool y) '())) (boolV (or x y))]
                                   [else (error 'interp "La operación sólo acepta booleanos como parámetro")])]
                  [(equal? f anD) (match lst
                                    [(cons (id i) xs) (boolV (and (cond
                                                                    [(boolean? (intV (lookup i ds))) (boolV-b (lookup i ds))]
                                                                    [else (error 'interp "La operación sólo acepta booleanos como parámetro")])
                                                                  (bool-b (first (map (lambda (c) (int-id-bool c ds)) xs)))))]
                                    [(cons (bool x) xs) (boolV (and x (bool-b (first (map (lambda (c) (int-id-bool c ds)) xs)))))]
                                    [(cons (bool x) (cons (bool y) '())) (boolV (and x y))]
                                    [else (error 'interp "La operación sólo acepta booleanos como parámetro")])]
                  [(equal? f not) (match lst
                                    [(cons (id i) '()) (boolV (not (cond
                                                                     [(boolean? (intV (lookup i ds))) (boolV-b (lookup i ds))]
                                                                     [else (error 'interp "La operación sólo acepta números como parámetro")])))]
                                    [(cons (bool x) '()) (boolV (not x))]
                                    [else (error 'interp "La operación sólo acepta booleanos como parámetro")])]
                  [(equal? f zero?) (match lst
                                      [(cons (id i) '()) (boolV (zero? (cond
                                                                         [(number? (intV (lookup i ds))) (numV-n (lookup i ds))]
                                                                         [else (error 'interp "La operación sólo acepta números como parámetro")])))]
                                      [(cons (num x) '()) (boolV (zero? x))]
                                      [else (error 'interp "La operación sólo acepta números como parámetro")])]
                  [(equal? f num?) (match lst
                                     [(cons (id i) '()) (boolV (num? (intV (lookup i ds))))]
                                     [(cons (num x) '()) (boolV (num? x))]
                                     [else (error 'interp "La operación no reconoce lo solicitado")])]
                  [(equal? f bool?) (match lst
                                      [(cons (id i) '()) (boolV (bool? (intV (lookup i ds))))]
                                      [(cons (bool x) '()) (boolV (bool? x))]
                                      [else (error 'interp "La operación no reconoce lo solicitado")])]
                  [(equal? f char?) (match lst
                                      [(cons (id i) '()) (boolV (char? (intV (lookup i ds))))]
                                      [(cons (chaR x) '()) (boolV (char? x))]
                                      [else (error 'interp "La operación no reconoce lo solicitado")])]
                  [(equal? f string?) (match lst
                                        [(cons (id i) '()) (boolV (string? (intV (lookup i ds))))]
                                        [(cons (strinG x) '()) (boolV (string? x))]
                                        [else (error 'interp "La operación no reconoce lo solicitado")])]
                  [(equal? f list?) (match lst
                                      [(cons (id i) '()) (boolV (list? (intV (lookup i ds))))]
                                      [(cons (lisT x) '()) (boolV (list? x))]
                                      [else (error 'interp "La operación no reconoce lo solicitado")])]
                  [(equal? f empty?) (match lst
                                       [(cons (lisT x) '()) (boolV (empty? x))]
                                       [else (error 'interp "La operación empty? sólo acepta una lista como parámetro")])]
                  [(equal? f cons) (match lst
                                     [(cons x (cons (id i) '())) (interp (lisT (cons x (lisT-l (int-id-lts (id i) ds)))) ds)]
                                     [(cons x (list (lisT xs))) (interp (lisT (cons x xs)) ds)]
                                     [else (error 'interp "La operación sólo acepta un elemento y una lista como parámetros")])]
                  [(equal? f car) (match lst
                                    [(cons (id i) '()) (cond
                                                         [(list? (intV (lookup i ds))) (cv (car (map (lambda (x) (intV x)) (listV-l (lookup i ds)))))]
                                                         [else (error "La operación sólo acepta listas como parámetro")])]
                                    [(list (lisT (cons x xs))) (interp x ds)]
                                    [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                  [(equal? f cdr) (match lst
                                    [(cons (id i) '()) (cond
                                                         [(list? (intV (lookup i ds))) (cv (cdr (map (lambda (x) (intV x)) (listV-l (lookup i ds)))))]
                                                         [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                                    [(list (lisT (cons x xs))) (interp (lisT xs) ds)]
                                    [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                  [(equal? f append) (match lst
                                       [(cons x '()) (interp x ds)]
                                       [(cons (lisT x) (cons (lisT y) ys)) (interp (op append (cons (appT (lisT x) (lisT y)) ys)) ds)]
                                       [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                  [(equal? f length) (match lst
                                       ['() (numV 0)]
                                       [(list '()) (numV 0)]
                                       [(list (lisT '())) (numV 0)]
                                       [(cons (lisT x) '()) (numV (lengthT (lisT x)))]
                                       [(cons (id i) '()) (cond
                                                            [(list? (intV (lookup i ds))) (cv (length (map (lambda (x) (intV x)) (listV-l (lookup i ds)))))]
                                                            [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                                       [else (error 'interp "La operación sólo acepta listas como parámetro")])]
                  [(equal? f string-append) (match lst
                                              [(cons x '()) (interp x ds)]
                                              [(cons (strinG x) '()) (interp (strinG x) ds)]
                                              [(cons (id i) '()) (cond
                                                                   [(list? (intV (lookup i ds))) (lookup i ds)]
                                                                   [else (error 'interp "La operación sólo acepta cadenas como parámatro")])]
                                              [(cons (id i) xs) (interp (op string-append (cons (strinG (cond
                                                                                                          [(string? (intV (lookup i ds))) (stringV-s (lookup i ds))]
                                                                                                          [else "La operación sólo acepta cadenas como paráetro"]))
                                                                                                (map (lambda(x) (int-id-str x ds)) xs))) ds)]
                                            
                                              ;[(cons (strinG i) xs) (interp (op string-append (cons (strinG i) (interp xs ds))) ds)]
                                              [(cons (strinG x) (cons (strinG y) ys)) (interp (op string-append (cons (strinG (string-append x y)) (map (lambda(x) (int-id-str x ds)) ys))) ds)]
                                              [(cons (strinG i) xs) (interp (op string-append (cons (strinG i) (map (lambda (x) (int-id-str x ds)) xs))) ds)]
                                              [else (error 'interp "La operación sólo acepta cadenas como parámetro")])]
                  [(equal? f string-length) (match lst
                                              [(cons (id i) '()) (cond
                                                                   [(string? (intV (lookup i ds))) (numV (string-length (stringV-s (lookup i ds))))]
                                                                   [else (error 'interp "La operación sólo acepta cadenas como parámetro")])]
                                              [(list (strinG a)) (numV (string-length a))]
                                              [else (error 'interp "La operación sólo acepta cadenas como parámetro")])])]
    [(fun param body) (closure param body ds)]
    [(app fun args) (let ([fun-val (interp fun ds)])
                      (interp (closure-body fun-val)
                              (aSub (first (closure-param fun-val))
                                    (interp-lst args ds)
                                    (closure-env fun-val))))]))



;; Concatena dos listas del tipo lisT
;; CFWBAE CFWBAE --> CFWBAE
(define (appT l1 l2)
  (match l1
    ['() l2]
    [(lisT '()) (l2)]
    [(lisT xs) (match l2
                 [(lisT '()) l1]
                 [(lisT ys) (lisT (appL xs ys))])]))

;; Concatena dos listas
;; appL :: CFWBAE CFWBAE --> CFWBAE
(define (appL l1 l2)
  (match l1
    ['() l2]
    [(cons x xs) (match l2
                   ['() l1]
                   [ys (cons x (appL xs ys))])]))

;; Calcula la longitud de una lista de tipo lisT
;; lengthT :: CFWBAE --> number
(define (lengthT sexp)
  (match sexp
    [(lisT '()) 0]
    [(lisT (cons x xs)) (+ 1 (lengthT (lisT xs)))]))

;; Interpreta todos los elementos de una lista de CFWBAE
;; interp-lst :: (listof CFWBAE) DefrdSub --> (listof CFWBAE-Value)
(define (interp-lst zs ds)
  (match zs
    ['() '()]
    [(cons x '()) (interp x ds)]
    [(cons x xs) (cons (interp x ds) (list (interp-lst xs ds)))]))

;; Busca en el ambiente un número y lo devuelve
;; int-id :: CFWBAE DefrdSub --> (CFWBAE)
(define (int-id sexp ds)
  (match sexp
    [(num n) sexp]
    [(id x) (cond
              [(number? (intV (lookup x ds))) (num (numV-n (lookup x ds)))]
              [else (error 'interp "La operación sólo acepta números como parámetro")])]))

;; Busca en el ambiente un booleano y lo devuelve
;; int-id-bool :: CFWBAE DefrdSub --> (CFWBAE)
(define (int-id-bool sexp ds)
  (match sexp
    [(bool n) sexp]
    [(id x) (cond
              [(bool? (intV (lookup x ds))) (bool (boolV-b (lookup x ds)))]
              [else (error 'interp "La operación sólo acepta booleanos como parámetro")])]))

;; Busca en el ambiente un caracter y lo devuelve
;; int-id-char :: CFWBAE DefrdSub --> (CFWBAE)
(define (int-id-char sexp ds)
  (match sexp
    [(chaR n) sexp]
    [(id x) (chaR (charV-c (lookup x ds)))]))

;; Busca en el ambiente una cadena y lo devuelve
;; int-id-str :: CFWBAE DefrdSub --> (CFWBAE)
(define (int-id-str sexp ds)
  (match sexp
    [(strinG n) sexp]
    [(id x) (cond
              [(string? (intV (lookup x ds))) (strinG (stringV-s (lookup x ds)))]
              [else (error 'interp "La operación sólo acepta cadenas como parámetro")])]))

;; Busca en el ambiente una lista y lo devuelve
;; int-id-lts :: CFWBAE DefrdSub --> (CFWBAE)
(define (int-id-lts sexp ds)
  (match sexp
    [(lisT n) sexp]
    [(id x) (cond
              [(list? (intV (lookup x ds))) (lisT (map (lambda (x) (cf (intV x))) (listV-l (lookup x ds))))]
              [else (error 'interp "La operación sólo acepta listas como parámetro")])]))

;; cf :: any --> CFWBAE
(define (cf exp)
  (cond
    [(number? exp) (num exp)]
    [(boolean? exp) (bool exp)]
    [(char? exp) (chaR exp)]
    [(string? exp) (strinG exp)]
    [(list? exp) (lisT (map (lambda (x) (cf x)) exp))]))

;; cf :: any --> CFWBAE-Value
(define (cv exp)
  (cond
    [(number? exp) (numV exp)]
    [(boolean? exp) (boolV exp)]
    [(char? exp) (charV exp)]
    [(string? exp) (stringV exp)]
    [(list? exp) (listV (map (lambda (x) (cv x)) exp))]))

;; Devuelve el parámetro de cada tipo de dato
;; intV :: CFWBAE-Value --> any
(define (intV expr)
  (match expr
    [(numV n) n]
    [(boolV b) b]
    [(charV c) c]
    [(stringV s) s]
    [(listV l) l]))

(require racket/trace)
(trace interp)
;(interp (desugar (parse '{length {1 2 3 4 5}})) (mtSub))

;(interp (op + '()) (mtSub))
;(interp (iF (bool #f) (num 6) (num 8)) (mtSub))
;(interp (op cons (list (num 4) (id 't))) (aSub 't (listV (list (charV #\b) (boolV #t))) (mtSub)))
;(interp (op anD (list (id 'p) (bool #f))) (aSub 'p (boolV #t) (mtSub)))
;(interp (op string-append (list (strinG "pe") (id 'v))) (aSub 'v (stringV "lo") (mtSub)))