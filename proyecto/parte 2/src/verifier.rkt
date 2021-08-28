#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta SRCFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof :: CFWBAE Type-Context --> Type
(define (typeof expr context)
  (match expr
    [(idS i) (find-type i context)]
    [(numS n) (numberT)]
    [(boolS b) (booleanT)]
    [(charS c) (charT)]
    [(stringS s) (stringT)]
    [(listS lst) (map (lambda (x) (typeof x context)) lst)]
    [(opS f lst) (cond
                   [(equal? f +) (cond
                                   [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                   [else (error 'typeof "tipos incorrectos")])]
                   [(equal? f -) (cond
                                   [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                   [else (error 'typeof "tipos incorrectos")])]
                   [(equal? f *) (cond
                                   [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                   [else (error 'typeof "tipos incorrectos")])]
                   [(equal? f /) (cond
                                   [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                   [else (error 'typeof "tipos incorrectos")])])]))

;; Busca el tipo correspondiente de un identificador
;; find-type :: SRCFWBAE Type-Context --> Type 
(define (find-type expr context)
  (match context
    [(phi) (error 'find-type "Identificador libre")]
    [(gamma id type rest) (cond
                            [(equal? id expr) type]
                            [else (find-type expr rest)])]))