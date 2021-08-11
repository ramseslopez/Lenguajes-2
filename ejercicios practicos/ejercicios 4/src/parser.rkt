#lang plai

(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWBAE
;; parse: s-expression -> CFWBAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp) (parse-list sexp)]))

(define (parse-list sexp)
  (case (car sexp)
    [(add1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
              [else (op add1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(sub1) (cond
              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
              [else (op sub1 (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(expt) (cond
              [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes")]
              [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
              [else (op expt (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(modulo) (cond
                [(<= (length sexp) 2) (error 'parse "No hay argumentos suficientes")]
                [(> (length sexp) 3) (error 'parse "Aridad incorrecta")]
                [else (op modulo (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(+) (cond
           [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
           [else (op + (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(-) (cond
           [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
           [else (op - (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(*) (cond
           [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
           [else (op * (map (lambda (x) (parse x)) (cdr sexp)))])]
    [(/) (cond
           [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
           [else (op / (map (lambda (x) (parse x)) (cdr sexp)))])]))
