#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> RCFWBAE-Typed
;; parse: s-expression -> RCFWBAE-Typed
(define (parse sexp)
 )
