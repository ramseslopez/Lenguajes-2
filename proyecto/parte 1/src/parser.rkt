#lang plai

(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A ::=  <number>
;;        | <symbol>
;;        | listof(A)
;; parse :: A --> CFWBAE
;; parse :: s-expression --> CFWBAE
(define (parse sexp)
    (cond
        [(symbol? sexp) (idS sexp)]
        [(number? sexp) (numS sexp)]
        [(boolean? sexp) (boolS sexp)]
        [(char? sexp) (charS sexp)]
        [(string? sexp) (stringS sexp)]
        [(list? sexp) (listS (map (lambda (x) (parse x)) sexp))]
        ))
