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
                   [(member f (list + - * / add1 sub1 modulo expt < <= = >= > zero?)) (cond
                                            [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                            [else (error 'typeof "Los tipos deben ser number")])]
                   [(member f (list anD oR not)) (cond
                                            [(andmap boolS? lst) (map (lambda (x) (typeof x context)) lst)]
                                            [else (error 'typeof "Los tipos deben ser boolean")])]
                   [(member f (list string-append string-length)) (cond
                                                    [(andmap stringS? lst) (map (lambda (x) (typeof x context)) lst)]
                                                    [else (error 'typeof "Los tipos deben ser string")])]
                   [(member f (list car cdr length append)) (cond
                                                              [(andmap listS? lst) (map (lambda (x) (typeof x context)) lst)]
                                                              [else (error 'typeof "Los tipos deben ser list")])]
                   [(member f (list cons)) (cond
                                      [(listS? (cdr lst)) (map (lambda (x) (typeof x context)) lst)]
                                      [else (error 'typeof "Debe haber un elemento y una lista")])]
                   [else (map (lambda (x) (typeof x context)) lst)])]))

;; Busca el tipo correspondiente de un identificador
;; find-type :: SRCFWBAE Type-Context --> Type 
(define (find-type expr context)
  (match context
    [(phi) (error 'find-type "Identificador libre")]
    [(gamma id type rest) (cond
                            [(equal? id expr) type]
                            [else (find-type expr rest)])]))
