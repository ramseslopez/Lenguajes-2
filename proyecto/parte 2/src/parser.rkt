#lang plai

(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A :: = <number>
;;      | <symbol>
;;      | listof(A)
;; parse :: A --> SRCFWBAE-Typed
;; parse :: s-expression --> SRCFWBAE-Typed
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(char? sexp) (charS sexp)]
    [(string? sexp) (stringS sexp)]
    [(list? sexp) (parse-aux sexp)]))

;; Parsea una lista s-expression a un ASA en SCFWBAE
;; parse-aux :: s-expression --> SCFWBAE
(define (parse-aux sexp)
  (match sexp
    ['() (void sexp)]
    [(cons x xs) (case (car sexp)
                   [(lst) (lsts sexp)]
                   [(if) (parse-if sexp)]
                   [(if0) (parse-if sexp)]
                   [(cond) (parse-if sexp)]
                   [(add1) (parse-num sexp)]
                   [(sub1) (parse-num sexp)]
                   [(modulo) (parse-num sexp)]
                   [(expt) (parse-num sexp)]
                   [(or) (parse-bool sexp)]
                   [(and) (parse-bool sexp)]
                   ((not) (parse-bool sexp))
                   [(<) (parse-ord sexp)]
                   [(<=) (parse-ord sexp)]
                   [(>=) (parse-ord sexp)]
                   [(>) (parse-ord sexp)]
                   [(zero?) (parse-pred sexp)]
                   [(num?) (parse-pred sexp)]
                   [(char?) (parse-pred sexp)]
                   [(bool?) (parse-pred sexp)]
                   [(string?) (parse-pred sexp)]
                   [(list?) (parse-pred sexp)]
                   [(empty?) (parse-pred sexp)]
                   [(+) (parse-num sexp)]
                   [(-) (parse-num sexp)]
                   [(*) (parse-num sexp)]
                   [(/) (parse-num sexp)]
                   [(cons) (parse-list sexp)]
                   [(car) (parse-list sexp)]
                   [(cdr) (parse-list sexp)]
                   [(append) (parse-list sexp)]
                   [(length) (parse-list sexp)]
                   [(string-length) (parse-str sexp)]
                   [(string-append) (parse-str sexp)]
                   [(fun) (parse-fun sexp)]
                   [(app) (parse-fun sexp)]
                   [(with) (parse-fun sexp)]
                   [(with*) (parse-fun sexp)]
                   [(rec) (parse-rec sexp)]
                   [else (listS (map (lambda (x) (parse x)) sexp))])]))

;; Parsea una lista de números s-expression a un ASA en SCFWBAE
;; parse-num :: s-expression --> SCFWBAE
(define (parse-num sexp)
  (case (car sexp)
    [(add1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [else (opS add1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(sub1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [else (opS sub1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(modulo) (cond
                [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
                [else (opS modulo (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(expt) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [else (opS expt (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(+) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [else (opS + (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(-) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [else (opS - (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(*) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [else (opS * (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(/) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [else (opS / (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de if's s-expression a un ASA en SCFWBAE
;; parse-if :: s-expression --> SCFWBAE
(define (parse-if sexp)
  (case (car sexp)
    [(if) (cond
            [(<= (length sexp) 3) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 4) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
            [else (iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))])]
    [(if0) (cond
             [(<= (length sexp) 3) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 4) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (iF0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))])]
    [(cond) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [else (cnds sexp)])]))

;; Parsea una lista de listass-expression a un ASA en SCFWBAE
;; parse-str :: s-expression --> SCFWBAE
(define (parse-str sexp)
  (case (car sexp)
    [(string-length) (cond
                       [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                       [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
                       [else (opS string-length (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(string-append) (cond
                       [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                       [else (opS string-append (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de listass-expression a un ASA en SCFWBAE
;; parse-list :: s-expression --> SCFWBAE
(define (parse-list sexp)
  (case (car sexp)
    [(cons) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [else (opS cons (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(car) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (opS car (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(cdr) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (opS cdr (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(append) (cond
                [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [else (opS append (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(length) (cond
                [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
                [else (opS length (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de booleanos s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-bool sexp)
  (case (car sexp)
    [(or) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
            [else (opS oR (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(and) (cond
             [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (opS anD (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(not) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (opS not (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de predicados s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-pred sexp)
  (case (car sexp)
    [(zero?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
               [else (opS zero? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(num?) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [else (opS num? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(char?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
               [else (opS char? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(bool?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
               [else (opS bool? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(string?) (cond
                 [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                 [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
                 [else (opS string? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(list?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
               [else (opS list? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(empty?) (cond
                [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 2) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
                [else (opS empty? (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de ordenes s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-ord sexp)
  (case (car sexp)
    [(<) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
           [else (opS < (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(<=) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
            [else (opS <= (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(>=) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
            [else (opS >= (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(>) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
           [else (opS > (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de funciones s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-fun sexp)
  (case (car sexp)
    [(fun) (cond
             [(<= (length sexp) 4) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 5) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (funS (params-list (second sexp)) (funT (map (lambda (x) (type x)) (no-arrows (fourth sexp)))) (parse (fifth sexp)))])]
          
    [(app) (cond
             [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
             [else (appS (parse (second sexp)) (map (lambda (x) (parse x)) (third sexp)))])]
    [(with) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
              [(<= (length (second sexp)) 1) (error 'parse "El identificador no posee un valor")]
              [(> (length (second sexp)) 2) (error 'parse "El identificador sólo puede recibir un valor")]
              [else (withs sexp)])]
    [(with*) (cond
               [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 3) (error 'parse "La cantidad de argumentos para realizar la operación solicitada es inválida")]
               [else (withS* (map (lambda (x) (binds x)) (second sexp)) (parse (third sexp)))])]))

(define (fst xs)
  (cond
    [(empty? xs) empty]
    [(symbol? xs) xs]
    [else (car xs)]))

(define (thd xs)
  (cond
    [(empty? xs) empty]
    [(symbol? xs) xs]
    [else (third xs)]))

(define (withs sexp)
  (case (car sexp)
    [(with) (cond
              [(symbol? (first (second sexp))) (cond
                                                 [(and (equal? (first (second sexp))
                                                          (first (third sexp)))
                                                       (equal? (first (second (second sexp))) 'fun))
                                                  (withS (list (bindingS (first (second sexp)) (funT (map (lambda (x) (type x)) (no-arrows (fourth (second (second sexp))))))
                                                                   (parse (second (second sexp)))))
                                                         (appS (idS (first (second sexp))) (listS-l (parse (second (third sexp))))))])
                        ]
              [else (withS
                     (list (bindingS (fst (first (second sexp)))
                                     (type (thd (first (second sexp))))
                                     (parse (second (second sexp)))))
                           (cond
                             [(and (equal? (first (second sexp)) (first (third sexp)))
                                   (equal? (first (second (second sexp))) 'fun))
                              (appS (idS (first (second sexp))) (listS-l (parse (second (third sexp)))))]
                             [else (parse (third sexp))]))])]))

; (funS (list (param 'x (numberT))) (funT (list (numberT) (numberT))) (opS #<procedure:*> (list (idS 'x) (numS 2))))

;; Parsea una lista de s-expression a un ASA
;; parse-rec :: s-expression --> SRCFWBAE
(define (parse-rec sexp)
  (case (car sexp)
    [(rec) (recS (list (bindingS (first (first (second sexp)))
                                 (funT (map (lambda (x) (type x)) (no-arrows (third (first (second sexp))))))
                                 (parse (fourth (first (second sexp)))))
                       (bindingS (first (second (second sexp)))
                                (type (third (second (second sexp))))
                                (parse (fourth(second (second sexp))))))
                 (parse (third sexp)))]))

;; Parsea una condicional s-expression a otra en SCFWBAE
;; cnds :: s-expression --> SRCFWBAE
(define (cnds sexp)
  (case (car sexp)
    [(cond) (condS (append (map (lambda (x)
                                  (condition (parse (first x)) (parse (second x)))) (no-last (cdr sexp)))
                           (list (else-cond (parse (second (last sexp)))))))]))

;; Elimina el último elemento de una lista
;; no-last :: s-expression --> s-expression
(define (no-last sexp)
  (reverse (cdr (reverse sexp))))

;; Transforma de s-expression a Binding
;; binds :: s-expression --> Binding
(define (binds sexp)
  (bindingS (first (first sexp)) (type (third (first sexp))) (parse (second sexp))))

;; Define un parámetro de una expresión
;; params :: s-expression -> Param
(define (params exp)
  (param (first exp) (type (third exp)))) 

;; Transforma a lista de parámetros
;; params-list :: (listof s-expressions) --> (listof Param)
(define (params-list lst)
  (match lst
    ['() empty]
    ;[(cons x '()) (params x)]
    [(cons x xs) (cons (params x) (params-list xs))]))

;; Obtiene los tipos de una lista de parámetros
;; get-types :: (listof Param) --> (listof Type)
(define (get-types lst)
  (match lst
    ['() '()]
    [(cons (param id type) xs) (cons type (get-types xs))]))

;; Parsea una lista a una lista con azúcar
;; lsts :: s-expression --> SCFWBAE
(define (lsts sexp)
  (case (car sexp)
    [(lst) (listS (map (lambda (x) (parse x)) (cdr sexp)))]))

;; Devuelve la lista vacía
;; void :: s-expression --> SRCFWBAE
(define (void sexp)
  (cond
    ['() (listS '())]))

;; Devuelve el tipo
;; type :: s-expression --> Type
(define (type exp)
  (cond 
    [(equal? exp 'number) (numberT)]
    [(equal? exp 'boolean) (booleanT)]
    [(equal? exp 'char) (charT)]
    [(equal? exp 'string) (stringT)]))

;; Elimina las flechas de una lista de tipos
;; no-arrows :: s-expression --> s-expression
(define (no-arrows lst)
  (match lst
    ['() '()]
    [(cons x xs) (cond
                   [(equal? x '->) (no-arrows xs)]
                   [else (cons x (no-arrows xs))])]))

#|(define (busca e lst)
  (cond
    [(empty? lst) #f]
    [(list? (car lst)) (cond
                         [(and (equal? (list? (car lst)) #t) (equal? (busca e (car lst)) #f) (equal? (empty? (car lst)) #f)) (busca e (car (car lst)))]
                         [else #t])]
    [(equal? e (car lst)) #t]
    [else (busca e (cdr lst))]))

(define (sym a b)
  (cond
    [(equal? (sym2str a) (sym2str b)) #t]
    [else #f]))

(define (sym2str s)
  (symbol->string s))|#

(define (2list lst)
  (match lst
    ['() empty]
    ;[]
    [(cons x xs) (cond
                   [(list? x) (cond
                                [(empty? xs) (2list x)]
                                [else (cons (2list x) (2list xs))])]
                   [else (cons x (2list xs))])]))

;(require racket/trace)
;(trace parse)