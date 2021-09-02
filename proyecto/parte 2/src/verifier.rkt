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
    [(opS f lst) (type-op f lst context)]
    [(iFS cnd thn els) (type-if expr context)]
    [(iF0 cnd thn els) (type-if expr context)]
    [(condS cnds) (type-cond cnds context)]
    [(withS lst body) (typeof body (type-with lst context))]
    [(withS* lst body) (typeof body (type-with lst context))]
    [(recS lst body) null]
    [(funS param type body) null]
    [(appS fun args) null]))

;; Busca el tipo correspondiente de un identificador
;; find-type :: SRCFWBAE Type-Context --> Type
(define (find-type expr context)
  (match context
    [(phi) (error "find-type: Free variable: " expr)]
    [(gamma id type rest) (cond
                            [(equal? id expr) type]
                            [else (find-type expr rest)])]))

;; Obtiene el tipo general de una operación
;; type-op :: procedure (listof SRCFWBAE) Type-Context --> Type
(define (type-op f lst context)
  (cond
    [(member f (list + - * / add1 sub1 modulo expt))
     (cond
       [(andmap (lambda (x) (numberT? (typeof x context))) lst) (numberT)]
       [else (error
              (string-append "typeof: Error in parameter: " (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(member f (list < <= = >= >))
     (cond
       [(andmap (lambda (x) (numberT? (typeof x context))) lst) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(equal? f zero?)
     (cond
       [(numberT? (typeof (car lst) context)) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(equal? f not)
     (cond
       [(booleanT? (typeof (car lst) context)) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "bool" context))
                             "\nExpected type: (booleanT)\nGiven type: " (~v (typeof (erroR lst "bool" context) context))))])]
    [(member f (list anD oR))
     (cond
       [(andmap (lambda (x) (booleanT? (typeof x context))) lst) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "bool" context))
                             "\nExpected type: (booleanT)\nGiven type: " (~v (typeof (erroR lst "bool" context) context))))])]
    [(equal? f string-append)
     (cond
       [(andmap (lambda (x) (stringT? (typeof x context))) lst) (stringT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "str" context))
                             "\nExpected type: (stringT)\nGiven type: " (~v (typeof (erroR lst "str" context) context))))])]
    [(equal? f string-length)
     (cond
       [(stringT? (typeof (car lst) context)) (numberT)]
       [else (error
              (string-append "typeof: Error in parameter:" (~v (erroR lst "str" context))
                             "\nExpected type: (stringT)\nGiven type: " (~v (typeof (erroR lst "str" context) context))))])]
    [(equal? f car) (typeof (car (listS-l (car lst))) context)]
    [(equal? f length) (cond
                         [(listS? (car lst)) (numberT)]
                         [else (error 'typeof "El tipo debe ser una lista")])]
    [(member f (list cons append)) (flatten (map (lambda (x) (typeof x context)) lst))]
    [(equal? f cdr) (map (lambda (x) (typeof x context)) (cdr (listS-l (car lst))))]
    [else (booleanT)]))

;; Obtiene el tipo general de una expresión if
;; type-if :: SRCFWBAE Type-Context --> Type
(define (type-if expr context)
  (match expr
    [(iFS cnd thn els)
     (cond
       [(booleanT? (typeof cnd context))
        (cond
          [(equal? (typeof thn context) (typeof els context)) (typeof thn context)]
          [else (error 'typeof "Type error\nConditionals must have same type in then-expr and else-expr")])]
       [else (error "if: Type error\nConditional's test-expr type must be a boolean\nGiven: " (typeof cnd context))])]
    [(iF0 cnd thn els)
     (cond
       [(numberT? (typeof cnd context))
        (cond
          [(equal? (typeof thn context) (typeof els context)) (typeof thn context)]
          [else (error 'typeof "Type error\nConditionals must have same type in then-expr and else-expr")])]
       [else (error "if0: Type error\nConditional's test-expr type must be a number\nGiven: " (typeof cnd context))])]))

;; Obtiene el tipo general de una expresión cond
;; type-cond :: SRCFWBAE Type-Context --> Type
(define (type-cond cnds context)
  (let ([prev-b '()])
    (cond
      [(andmap (lambda (x)
                 (match x
                   [(condition a b)
                    (cond
                      [(booleanT? (typeof a context))
                       (let ([type-b (typeof b context)])
                         (cond
                           [(or (equal? prev-b type-b) (equal? prev-b '())) #t]
                           [else (error 'typeof "Type error\nConditionals must have same type in then-expr's")])
                         (set! prev-b type-b))]
                      [else (error "cond: Type error\nConditional's test-expr type must be a number\nGiven: " (typeof a context))])]
                   [(else-cond c)
                    (cond
                      [(equal? prev-b (typeof c context)) #t]
                      [else (error 'typeof "Type error\nConditionals must have same type in then-expr and else-expr")])])) cnds) prev-b]
      [else (error 'type-of "Type error\nConditionals")])))

;; Obtiene el tipo general de una expresión with
;; type-with :: (listof BindingS) Type-Context --> Type-Context
(define (type-with lst context)
  (match lst
    ['() context]
    [(cons (bindingS id type val) xs)
     (cond
       [(equal? type (typeof val context)) (type-with xs (gamma id type context))]
       [else (error (string-append "typeof: Type Error\nExpected type: " (~v type)
                                   "\nGiven type: " (~v (typeof val context))))])]))


;; Obtiene el tipo general de una función
;; type-fun ::
(define (type-fun b) null)

;; Obtiene el tipo general de uan función recursiva
;; type-rec :: (listof BindingS) s-expression Type-Context --> Type
(define (type-rec lst body context)
  (match lst
    ['() (typeof body context)]
    [(cons (bindingS id type val) xs) (cond
                                        [(equal? (typeof val context) type) (type-rec xs body context)]
                                        [else (error 'typeof "El tipo del value es incorrecto")])]))

;; Obtiene el parámetro que no cumple una propiedad específica
;; erroR :: (listof SRCFWBAE) string Type-Context --> SRCFWBAE
(define (erroR lst type context)
  (match type
    ["num" (cond
               [(empty? lst) empty]
               [(not (numberT? (typeof (car lst) context))) (car lst)]
               [else (erroR (cdr lst) "num" context)])]
    ["bool" (cond
               [(empty? lst) empty]
               [(not (booleanT? (typeof (car lst) context))) (car lst)]
               [else (erroR (cdr lst) "bool" context)])]
    ["char" (cond
               [(empty? lst) empty]
               [(not (charT? (typeof (car lst) context))) (car lst)]
               [else (erroR (cdr lst) "char" context)])]
    ["str" (cond
               [(empty? lst) empty]
               [(not (stringT? (typeof (car lst) context))) (car lst)]
               [else (erroR (cdr lst) "str" context)])]))

;(require racket/trace)
;(trace type-with)
;(trace type-with-aux)
;(typeof (parse '{with [((x : number) number) ((y : number) 9)] {* x y}}) (phi))

#|
(define (type-cond cnd context)
  (let ([prev-b '()])
    (if (andmap (lambda (x) (match x
                              [(condition a b) (if (booleanT? (typeof a context))
                                                   (let ([t (typeof b context)])
                                                     (if (or (equal? prev-b t)
                                                             (equal? prev-b '()))
                                                         #t
                                                         (error 'typeof "Type error\nConditionals must have same type in then-expr's"))
                                                     (set! prev-b t))
                                                   (error "cond: Type error\nConditional's test-expr type must be a number\nGiven: " (typeof a context)))]
                              [(else-cond c) (if (equal? prev-b (typeof c context))
                                                 #t
                                                 (error 'typeof "Type error\nConditionals must have same type in then-expr and else-expr"))])) cnd)
        prev-b
        (error 'type-of "Type error\nConditionals"))))|#
