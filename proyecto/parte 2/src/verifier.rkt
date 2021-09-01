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
                   [(or (equal? f +) (equal? f -) (equal? f *) (equal? f /)
                        (equal? f add1) (equal? f sub1) (equal? f modulo) (equal? f expt)
                        (equal? f <) (equal? f <=) (equal? f =) (equal? f >=) (equal? f >)
                        (equal? f zero?)) (cond
                                            [(andmap numS? lst) (map (lambda (x) (typeof x context)) lst)]
                                            [else (error 'typeof "tipos incorrectos")])]
                   [(or (equal? f anD) (equal? f oR)
                        (equal? f not)) (cond
                                            [(andmap boolS? lst) (map (lambda (x) (typeof x context)) lst)]
                                            [else (error 'typeof "tipos incorrectos")])]
                   [(or (equal? f string-append)
                        (equal? f string-length)) (cond
                                                    [(andmap stringS? lst) (map (lambda (x) (typeof x context)) lst)]
                                                    [else (error 'typeof "tipos incorrectos")])]
                   [(or (equal? f car) (equal? f cdr)
                       (equal? f length) (equal? f append)) (cond
                                                              [(andmap listS? lst) (map (lambda (x) (typeof x context)) lst)]
                                                              [else (error 'typeof "tipos incorrectos")])]
                   [(equal? f cons) (cond
                                      [(listS? (cdr lst)) (map (lambda (x) (typeof x context)) lst)]
                                      [else (error 'typeof "tipos incorrectos")])]
                   [else (map (lambda (x) (typeof x context)) lst)])]))

;; Busca el tipo correspondiente de un identificador
;; find-type :: SRCFWBAE Type-Context --> Type 
(define (find-type expr context)
  (match context
    [(phi) (error 'find-type "Identificador libre")]
    [(gamma id type rest) (cond
                            [(equal? id expr) type]
                            [else (find-type expr rest)])]))