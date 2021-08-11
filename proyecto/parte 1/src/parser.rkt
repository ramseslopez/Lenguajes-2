#lang plai

(require (file "./grammars.rkt"))

;; Toma una lista de números, símbolos o listas
;; y la traduce a un árbol de sintaxis abstracta SCFWBAE
;; A ::=  <number>
;;        | <symbol>
;;        | listof(A)
;; parse :: A --> SCFWBAE
;; parse :: s-expression --> SCFWBAE
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
                   [else (listS (map (lambda (x) (parse x)) sexp))])]))

;; Parsea una lista de números s-expression a un ASA en SCFWBAE
;; parse-num :: s-expression --> SCFWBAE
(define (parse-num sexp)
  (case (car sexp)
    [(add1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
              [else (opS add1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(sub1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
              [else (opS sub1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(modulo) (cond
                [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
                [else (opS modulo (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(expt) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
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
            [(> (length sexp) 4) (error 'parse "Aridad incorrecta")]
            [else (iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))])]
    [(if0) (cond
             [(<= (length sexp) 3) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 4) (error 'parse "Aridad incorrecta")]
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
                       [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
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
              [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
              [else (opS cons (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(car) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
             [else (opS car (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(cdr) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
             [else (opS cdr (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(append) (cond
                [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [else (opS append (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(length) (cond
                [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                [else (opS length (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de booleanos s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-bool sexp)
  (case (car sexp)
    [(or) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
            [else (opS oR (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(and) (cond
             [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
             [else (opS anD (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(not) (cond
             [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
             [else (opS not (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de predicados s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-pred sexp)
  (case (car sexp)
    [(zero?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
               [else (opS zero? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(num?) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
              [else (opS num? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(char?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
               [else (opS char? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(bool?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
               [else (opS bool? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(string?) (cond
                 [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                 [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                 [else (opS string? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(list?) (cond
               [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
               [else (opS list? (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(empty?) (cond
                [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes para realizar la operación")]
                [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                [else (opS empty? (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de ordenes s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-ord sexp)
  (case (car sexp)
    [(<) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
           [else (opS < (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(<=) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
            [else (opS <= (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(>=) (cond
            [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
            [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
            [else (opS >= (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(>) (cond
           [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
           [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
           [else (opS > (map (lambda (x) (parse x)) (cdr sexp)))])]))

;; Parsea una lista de funciones s-expression a un ASA en SCFWBAE
;; parse-bool :: s-expression --> SCFWBAE
(define (parse-fun sexp)
  (case (car sexp)
    [(fun) (cond
             [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
             [else (funS (second sexp) (parse (third sexp)))])]
    [(app) (cond
             [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
             [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
             [else (appS (parse (second sexp)) (map (lambda (x) (parse x)) (third sexp)))])]
    [(with) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
              [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
              [(<= (length (second sexp)) 1) (error 'parse "El identificador no posee un valor")]
              [(> (length (second sexp)) 2) (error 'parse "El identificador sólo puede recibir un valor")]
              [else (withS (list (binding (first (second sexp)) (parse (second (second sexp))))) (parse (third sexp)))])]
    [(with*) (cond
               [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes para realizar la operación")]
               [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
               [else (withS* (map (lambda (x) (binds x)) (second sexp)) (parse (third sexp)))])]))



;; Parsea una condicional s-expression a otra en SCFWBAE
;; cnds :: s-expression --> SCFWBAE
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
  (binding (first sexp) (parse (second sexp))))

;; Parsea una lista a una lista con azúcar
;; lsts :: s-expression --> SCFWBAE
(define (lsts sexp)
  (case (car sexp)
    [(lst) (listS (map (lambda (x) (parse x)) (cdr sexp)))]))

;; Verifica si una lista es vacía y la parsea
;; void :: s-expression --> SCFWBAE
(define (void sexp)
  (cond
    ['() (listS '())]))
