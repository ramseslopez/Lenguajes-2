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
    [(string? sexp (stringS sexp))]
    [(list? sexp) (parse-aux sexp)]))

;;
;; parse-aux :: s-expresson --> SRCFWBAE
(define (parse-aux sexp)
  (match sexp
    ['() (void sexp)]))

;;
;; void :: s-expression --> SRCFWBAE
(define (void sexp)
  (cond
    ['() (listS '())]))