#lang plai

(require (file "./grammars.rkt"))

(define anD (lambda x  
    (if (null? x)
        #t
        (if (car x) (apply anD (cdr x)) #f))))

(define oR (lambda x
    (if (null? x)
        #f
        (or (car x) (apply oR (cdr x))))))

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
    [(empty? sexp) (error 'parse "Las expresiones vacías no son válidas")]
    [(list? sexp) (parse-aux sexp)]))

;; Parsea una lista s-expression a un ASA en SCFWBAE
;; parse-aux :: s-expression --> SCFWBAE
(define (parse-aux sexp)
  (match sexp
    ['() (void sexp)]
    [(cons x xs) (case (car sexp)
                   [(sub1 add1 not length car cdr string-length) (if (equal? (length sexp) 2)
                                                                     (opS (quitar-lista (car sexp)) (list (parse (second sexp))))
                                                                     (error 'parse "La aridad debe ser de 1")
                                                                     )]
                   [(num? char? bool? string? list? empty? zero?) (if (equal? (length sexp) 2)
                                                                      (opS (quitar-lista (car sexp)) (map parse (cdr sexp)))
                                                                      (error 'parse "La aridad debe ser de 1"))]
                   [(modulo expt) (if (equal? (length sexp) 3)
                                      (opS (quitar-lista (car sexp)) (list (parse (second sexp)) (parse (third sexp))))
                                      (error 'parse "La aridad debe ser de 2"))]
                   [(cons) (if (equal? (length sexp) 3)
                               (opS (quitar-lista (car sexp)) (map parse (cdr sexp)))
                               (error 'parse "La aridad debe ser de 2"))]
                   [(+ - * / < <= = > >= append string-append) (opS (quitar-lista (car sexp)) (map parse (cdr sexp)))]
                   [(and) (opS anD (map parse (cdr sexp)))]
                   [(or) (opS oR (map parse (cdr sexp)))]
                   [(lst) (listS (map parse (cdr sexp)))]
                   [(if) (if (equal? (length sexp) 4)
                             (iFS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))
                             (error 'parse "La aridad debe ser de 3"))]
                   [(if0) (if (equal? (length sexp) 4)
                              (iF0 (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))
                              (error 'parse "La aridad debe ser de 3"))]
                   [(cond) (if (<= (length sexp) 1)
                               (error 'parse "La aridad debe ser mayor a 1")
                               (if (check-else sexp)
                                   (condS (map (lambda x (aux-cond (car x))) (cdr sexp)))
                                   (error 'parse "Debe haber un else al final del cond")))]
                   [(with) (withS (with-aux (second sexp)) (parse (third sexp)))]
                   [(with*) (withS* (with-aux (second sexp)) (parse (third sexp)))]
                   [(fun) (if (tiene-repetidos (second sexp))
                              (error 'parse "La función no puede tener parámetros repetidos")
                              (funS (second sexp) (parse (third sexp))))]
                   [(app) (appS (parse (second sexp)) (map parse (third sexp)))]
                   [(lst) (listS (map parse sexp))]
                   [else (appS (parse (first sexp)) (map parse (second sexp)))])]))

;; Función que quita la lista y devuelve el operador
(define (quitar-lista car-sexp)
  (case car-sexp
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(<=) <=]
    [(=) =]
    [(>) >]
    [(>=) >=]
    [(sub1) sub1]
    [(add1) add1]
    [(not) not]
    [(length) length]
    [(car) car]
    [(cdr) cdr]
    [(string-length) string-length]
    [(num?) num?]
    [(char?) char?]
    [(bool?) bool?]
    [(string?) string?]
    [(list?) list?]
    [(empty?) empty?]
    [(zero?) zero?]
    [(modulo) modulo]
    [(expt) expt]
    [(cons) cons]
    [(append) append]
    [(string-append) string-append]))

;; Función auxiliar que parsea un condicional hasta encontrar
;; la sentencia `else`
(define (aux-cond sexp)
  (case (car sexp)
    [(else) (else-cond (parse (second sexp)))]
    [else (condition (parse (car sexp))
                     (parse (second sexp)))]))

;; Función auxiliar que parsea lista de bindings
(define (with-aux bindings)
  (map (lambda (b)
         (binding (first b)
                  (parse (cadr b)))) bindings))

;; Función que verifica si una lista tiene elementos repetidos
(define (tiene-repetidos lst)
  (if (equal? (check-duplicates lst) #f) #f #t))

;; Checa si el último elemento de un cond es un else
(define (check-else sexp)
	(equal? (car (last sexp)) 'else))
