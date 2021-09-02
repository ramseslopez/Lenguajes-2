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
                   [(member f (list + - * / add1 sub1 modulo expt)) (cond
                                            [(andmap (lambda (x) (numberT? (typeof x context))) lst) (numberT)]
                                            [else (error (~a "typeof: Error in parameter: " (erroR lst "num") "\nExpected type: (numberT)\nGiven type: " (erroR lst "num")))])]
                   [(member f (list < <= = >= >)) (cond
                                                    [(andmap (lambda (x) (numberT? (typeof x context))) lst) (booleanT)]
                                                    [else (error 'typeof "Los tipos deben ser number")])]
                   [(equal? f zero?) (cond
                                       [(numberT? (typeof (car lst) context)) (booleanT)]
                                       [else (error 'typeof "El tipo debe ser number")])]
                   [(equal? f not) (cond
                                       [(booleanT? (typeof (car lst) context)) (booleanT)]
                                       [else (error 'typeof "El tipo debe ser boolean")])]
                   [(member f (list anD oR)) (cond
                                            [(andmap (lambda (x) (booleanT? (typeof x context))) lst) (booleanT)]
                                            [else (error 'typeof "Los tipos deben ser boolean")])]
                   [(equal? f string-append) (cond
                                                    [(andmap (lambda (x) (stringT? (typeof x context))) lst) (stringT)]
                                                    [else (error 'typeof "Los tipos deben ser string")])]
                   [(equal? f string-length) (cond
                                       [(stringT? (typeof (car lst) context)) (numberT)]
                                       [else (error 'typeof "El tipo debe ser string")])]
                   [(equal? f car) (typeof (car (listS-l (car lst))) context)]
                   [(equal? f length) (cond
                                        [(listS? (car lst)) (numberT)]
                                        [else (error 'typeof "El tipo debe ser una lista")])]
                   [(member f (list cons append)) (flatten (map (lambda (x) (typeof x context)) lst))]
                   [(equal? f cdr) (map (lambda (x) (typeof x context)) (cdr (listS-l (car lst))))]
                   [else (booleanT)])]
    [(iFS cnd thn els) (cond
                         [(booleanT? (typeof cnd context)) (cond
                                                             [(equal? (typeof thn context) (typeof els context)) (typeof thn context)]
                                                             [else
                                                              (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr")])]
                         [else (error "if: Type error\nConditional's test-expr type must be a boolean\nGiven: " (typeof cnd context))])]
    [(iF0 cnd thn els) (cond
                         [(numberT? (typeof cnd context)) (cond
                                                             [(equal? (typeof thn context) (typeof els context)) (typeof thn context)]
                                                             [else
                                                              (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr")])]
                         [else (error "if: Type error\nConditional's test-expr type must be a boolean\nGiven: " (typeof cnd context))])]
    [(condS cnds) (type-cond cnds context)]))

;; Busca el tipo correspondiente de un identificador
;; find-type :: SRCFWBAE Type-Context --> Type 
(define (find-type expr context)
  (match context
    [(phi) (error 'find-type "Identificador libre")]
    [(gamma id type rest) (cond
                            [(equal? id expr) type]
                            [else (find-type expr rest)])]))

(define (get-cond cnd n)
  (match cnd
    [(condition a b) (if (= n 1)
                         a
                         b)]
    [(else-cond c) (if (= n 1)
                       c
                       #f)]))

#|(define (type-cond cnd context prev-cnd)
  (map (lambda (x) (match cnd
                     [(condition a b) (if (booleanT? (typeof a context))
                                          (if (or (equal? prev-cnd (typeof b context)) (equal? prev-cnd "N"))
                                              (type-cond )
                                              ())
                                          ())])) ))|#

(define (type-cond cnd context)
  (let ([prev-b '()])
    (if (andmap (lambda (x) (match x
                              [(condition a b) (if (booleanT? (typeof a context))
                                                   (let ([t (typeof b context)])
                                                     (if (or (equal? prev-b t)
                                                             (equal? prev-b '()))
                                                         #t
                                                         (error "type-cond: then tiene tipos distintos: " b))
                                                     (set! prev-b t))
                                                   (error "type-cond: condition no es boolean: " a))]
                              [(else-cond c) (if (equal? prev-b (typeof c context))
                                                 #t
                                                 (error "type-cond: else tiene distinto tipo " c))])) cnd)
        prev-b
        (error 'type-cond "error de tipo"))))


(define (erroR lst type)
  (match type
    ["num" (cond
               [(empty? lst) empty]
               [(not (numS? (car lst))) (car lst)]
               [else (erroR (cdr lst) "num")])]
    ["bool" (cond
               [(empty? lst) empty]
               [(not (boolS? (car lst))) (car lst)]
               [else (erroR (cdr lst) "bool")])]
    ["char" (cond
               [(empty? lst) empty]
               [(not (charS? (car lst))) (car lst)]
               [else (erroR (cdr lst) "char")])]
    ["str" (cond
               [(empty? lst) empty]
               [(not (stringS? (car lst))) (car lst)]
               [else (erroR (cdr lst) "str")])]))

#|cond
    [(empty? lst) empty]
    [(not (numS? (car lst))) (car lst)]
    [else (erroR (cdr lst))]|#
