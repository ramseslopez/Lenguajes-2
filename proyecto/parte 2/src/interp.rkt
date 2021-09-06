#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;;Función para obtener una nueva localización para la siguiente caja.
;;No recibe parámetros
;; new-location :: --> Number
(define new-location
  (let ([n (box 0)])
    (λ ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))
    

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup :: symbol Env --> RCFWBAE-Typed
(define (lookup name ds)
  (match ds
    [(mtSub) (error (string-append "lookup: Hay un identificador libre: " (symbol->string name)))]
    [(aSub id value env) (cond
                           [(equal? name id) value]
                           [else (lookup name env)])]
    [(aRecSub id value rest-env)
               (if (equal? id name)
                       (unbox value) 
                      (lookup name rest-env))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp :: RCFWBAE-Typed Env --> RCFWBAE-Value
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
                         [else (interp (iF (bool (cond
                                                   [(boolV? (interp cnd ds)) (boolV-b (interp cnd ds))]
                                                   [else (error 'interp "Símbolo no esperado. La condicional de if, no es un booleano")]))
                                           then els) ds)])]
    [(op f lst) (let* ([a (map (lambda (x) (cond
                                             [(numV? (interp x ds)) (numV-n (interp x ds))]
                                             [(boolV? (interp x ds)) (boolV-b (interp x ds))]
                                             [(charV? (interp x ds)) (charV-c (interp x ds))]
                                             [(stringV? (interp x ds)) (stringV-s (interp x ds))]
                                             [(listV? (interp x ds))
                                              (map (lambda (y) (get-param y)) (listV-l (interp x ds)))])) lst)]
                       [b (verified-args f a)])
                  (cond
                    [(number? b) (numV b)]
                    [(boolean? b) (boolV b)]
                    [(char? b) (charV b)]
                    [(string? b) (stringV b)]
                    [(list? b) (listV (map (lambda (z) (interp (to-rcfwbae z) ds)) b))]))]
    [(rec lst body) (interp body (cyclically-bind-and-interp lst ds))]
    [(fun param body) (closure (map get-symbol param) body ds)]
    [(app fun args) (let ([fun-val (interp fun ds)])
                      (interp (closure-body fun-val)
                              (interp-app (closure-param fun-val) args (closure-env fun-val) ds)))]))

;; Tranforma un elemento a uno de tipo CFWBAE
;; cf :: any --> CFWBAE
(define (to-rcfwbae exp)
  (cond
    [(number? exp) (num exp)]
    [(boolean? exp) (bool exp)]
    [(char? exp) (chaR exp)]
    [(string? exp) (strinG exp)]
    [(list? exp) (lisT (map (lambda (x) (to-rcfwbae x)) exp))]))

;; Devuelve el parámetro de cada tipo de dato
;; get-param :: CFWBAE-Value --> any
(define (get-param expr)
  (match expr
    [(numV n) n]
    [(boolV b) b]
    [(charV c) c]
    [(stringV s) s]
    [(listV l) l]))


;; Verifica si los operadores poseen los tipos correctos
;; verified-args :: procedure (listof any) --> any
(define (verified-args f args)
  (cond 
    [(member f (list + - * / modulo expt < <= = >= > zero? add1 sub1)) (cond
                                                                         [(andmap number? args)  (apply f args)]
                                                                         [else (error 'interp "El operador sólo acepta números")])]
    [(member f (list anD oR not)) (cond
                                    [(andmap boolean? args) (apply f args)]
                                    [else (error 'interp "El operador sólo acepta booleanos")])]
    [(member f (list string-append string-length)) (cond
                                                     [(andmap string? args) (apply f args)]
                                                     [else (error 'interp "El operador sólo acepta cadenas")])]
    [(member f (list append length cdr)) (cond
                                               [(andmap list? args) (apply f args)]
                                               [else (error 'interp "El operador sólo acepta listas")])]
    [(equal? f car) (cond
                      [(andmap list? args) (apply f args)]
                      [else (error 'interp "Símbolo no esperado. El segundo parámetro de car debe ser una lista.")])]
    [(member f (list cons)) (cond
                              [(list? (cdr args)) (apply f args)]
                              [else (error 'interp "El operador sólo acepta un elemento y una lista")])]
    [else (apply f args)]))

;; Función que acarrea los id y valores en el ambiente
;; interp-app :: (listof symbol) (listof CFWBAE-Value) DefrdSub DefrdSub --> DefrdSub 
(define (interp-app param arg env ds)
  (cond
    [(and (empty? param) (empty? arg)) env]
    [(equal? (length param) (length arg))
     (interp-app (cdr param) (cdr arg) (aSub (first param) (interp (first arg) ds) env) ds)]
    [else (error 'interp "La cantidad de parámetros y agumentos debe ser la misma")]))

(define (get-symbol par)
  (match par
    [(param id type) id]))


;; cyclically-bind-and-interp : symbol RCFWBAE Env --> Env
(define (cyclically-bind-and-interp lst env)
  (match lst
    ['() env]
    [(cons (binding id value) xs) (let* ([contenedor (box 'dummy)] 
                                         [ambiente (aRecSub id contenedor env)]
                                         [valor (interp value ambiente)])
                                    (begin
                                      (set-box! contenedor valor) 
                                      (cyclically-bind-and-interp xs ambiente)))]))

#|(define (test n)
  (let ([fact (box 'dummy)])
    (let ([fact-fun
           (lambda n
             (if (zero? n)
                 1
                 (* n ((unbox fact) (- n 1)))))])
      (begin
        (set-box! fact fact-fun)
        ((unbox fact) n)))))

(test 5)|#
;(require racket/trace)
;(trace interp)
;(trace test)
;(interp (desugar (parse '{rec ([fac : (number -> number) {fun {(n : number)} : (number -> number) {if {zero? n} 1 {* n {fac ({- n 1})}}}}] [n : number 5]) {fac (n)}})) (mtSub))
;(interp (desugar (parse '{rec ([fibo : (number -> number) {fun {(n : number)} : (number -> number) {if {zero? n} 0 {if {= n 1} 1 {+ (fibo {(- n 1)}) (fibo {(- n 2)})}}}}] [n : number 10]) {fibo (n)}})) (mtSub))