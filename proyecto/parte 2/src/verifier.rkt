#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta SRCFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof :: CFWBAE Type-Context --> Type
(define (typeof expr context)
  (type-case SRCFWBAE-Typed expr
    [idS (i) (find-type i context)]
    [numS (n) (numberT)]
    [boolS (b) (booleanT)]
    [charS (c) (charT)]
    [stringS (s) (stringT)]
    [listS (lst) (listT)]
    [opS (f lst) (type-op f lst context)]
    [iFS (cnd thn els) (type-if expr context)]
    [iF0 (cnd thn els) (type-if expr context)]
    [condS (cnds) (type-cond cnds context)]
    [withS (lst body) (typeof body (type-with lst context))]
    [withS* (lst body) (typeof body (type-with lst context))]
    [recS (lst body) (cond
                       [(and (comp-in-out lst) (void? (type-recs lst context))) (get-las-type (bindingS-type (car lst)))]
                       [else (error 'typeof "Type error")])]
    [funS (param type body) (type-fun param type body context)]
    [appS (fun args) (type-app fun args context)]))

(define (comp-in-out lst)
  (match lst
    ['() #t]
    [(cons (bindingS id type value) xs) (match type
                                          [(funT param) (equal? (take param (- (length param) 1)) (get-aux xs))])]))

(define (get-aux lst)
  (match lst
    ['() '()]
    [(cons (bindingS id type val) xs) (cons type (get-aux xs))]))


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
              (string-append "typeof: Error in parameter " (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(member f (list < <= = >= >))
     (cond
       [(andmap (lambda (x) (numberT? (typeof x context))) lst) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(equal? f zero?)
     (cond
       [(numberT? (typeof (car lst) context)) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "num" context))
                             "\nExpected type: (numberT)\nGiven type: " (~v (typeof (erroR lst "num" context) context))))])]
    [(equal? f not)
     (cond
       [(booleanT? (typeof (car lst) context)) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "bool" context))
                             "\nExpected type: (booleanT)\nGiven type: " (~v (typeof (erroR lst "bool" context) context))))])]
    [(member f (list anD oR))
     (cond
       [(andmap (lambda (x) (booleanT? (typeof x context))) lst) (booleanT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "bool" context))
                             "\nExpected type: (booleanT)\nGiven type: " (~v (typeof (erroR lst "bool" context) context))))])]
    [(equal? f string-append)
     (cond
       [(andmap (lambda (x) (stringT? (typeof x context))) lst) (stringT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "str" context))
                             "\nExpected type: (stringT)\nGiven type: " (~v (typeof (erroR lst "str" context) context))))])]
    [(equal? f string-length)
     (cond
       [(stringT? (typeof (car lst) context)) (numberT)]
       [else (error
              (string-append "typeof: Error in parameter " (~v (erroR lst "str" context))
                             "\nExpected type: (stringT)\nGiven type: " (~v (typeof (erroR lst "str" context) context))))])]
    [(equal? f car) (typeof (car (listS-l (car lst))) context)]
    [(equal? f length) (cond
                         [(listT? (typeof (car lst) context)) (numberT)]
                         [else
                          (error (string-append "typeof: Error in parameter " (~v (erroR lst "lts" context))
                                                "\nExpected type: (listT)\nGiven type: " (~v (typeof (erroR lst "lts" context) context))))])]
    [(member f (list cons append)) (listT)]
    [(equal? f cdr) (listT)]
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
          [else (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr")])]
       [else (error "if: Type error\nConditional's test-expr type must be a boolean\nGiven:"(typeof cnd context))])]
    [(iF0 cnd thn els)
     (cond
       [(numberT? (typeof cnd context))
        (cond
          [(equal? (typeof thn context) (typeof els context)) (typeof thn context)]
          [else (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr")])]
       [else (error "if0: Type error\nConditional's test-expr type must be a number\nGiven:"(typeof cnd context))])]))

;; Obtiene el tipo general de una expresión cond
;; type-cond :: SRCFWBAE Type-Context --> Type
(define (type-cond cnds context)
  (let* ([prev-b '()]
         [cond-list? (lambda (x)
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
                           [else (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr")])]))])
    (cond
      [(andmap cond-list? cnds) prev-b]
      [else (error 'type-of "Error\nConditionals")])))



;; Obtiene el tipo general de una expresión with
;; type-with :: (listof BindingS) Type-Context --> Type-Context
(define (type-with lst context)
  (match lst
    ['() context]
    [(cons (bindingS id type val) xs)
     (cond
       [(equal? (get-las-type type) (typeof val context)) (type-with xs (gamma id type context))]
       [else (error (string-append "typeof: Type Error\nExpected type: " (~v type)
                                   "\nGiven type: " (~v (typeof val context))))])]))




(define (type-recs lst context)
  (match lst
    ['() context]
    [(cons (bindingS id type val) xs)
     (cond
       [(equal? id (free-var id context)) (cond 
                                            [(equal? (get-las-type type) (typeof val (gamma id (get-las-type type) context)))
                                             (type-recs xs (gamma id type (gamma id (get-las-type type) context)))])]
       [(equal? (get-las-type type) (typeof val context)) (type-recs xs (gamma id type context))]
       [else (error (string-append "typeof: Type Error\nExpected type: " (~v type)
                                   "\nGiven type: " (~v (typeof val context))))])]))

(define (free-var id context)
  (match context
    [(phi) id]
    [(gamma var type rest) (cond
                             [(equal? id var) #f]
                             [else (free-var id rest)])]))

;; Obtiene el tipo general de una función
;; type-fun :: (listof Param) Type SRCFWBAE Type-Context --> Type
(define (type-fun lst type body context)
    (let* ([get-type (lambda (x) (match x
                                   [(param id type) type]))]
           [param-type (map (lambda (y)
                              (get-type y)) lst)])
      (cond
        [(equal? (get-las-type type) (typeof body (fparam lst context)))
         (funT (append param-type (list (get-las-type type))))]
        [else (error "fun: Type Error\n type and body must be the same")])))

(define (get-las-type type)
  (match type
    [(funT param) (last param)]
    [else type]))

;; Acumula los identificadores en el contexto
;; fparam :: (listof Param) Context-Type --> Context-Type
(define (fparam lst context)
  (match lst
    ['() context]
    [(cons x xs) (match x
                   [(param id type) (fparam xs (gamma id type context))])]))

;; Obtiene el tipo general de uan función recursiva
;; type-rec :: (listof BindingS) s-expression Type-Context --> Type
(define (type-rec lst context)
  (match lst
    ['() context]
    [(cons (bindingS id type val) xs) (gamma id val (type-rec xs context))]))


(define (type-rec-aux lst context)
  (match lst
    [(cons (bindingS id type val) xs) (cond
                                       [(equal? (not (member id (flatten val))) #f) (gamma id type (type-rec xs context))]
                                       [else (error "ERROR")])]))

(define (type-app fun args context)
  (let* ([type-args (map (lambda (x) (typeof x context)) args)]
         [type-fun (typeof fun context)]
         [type-params (cond
                        [(funS? fun) (funS-params fun)]
                        [else void])])
    (cond
      [(not (list? type-params)) (car type-args)]
      [(equal? (length type-params) (length type-args)) (if (equal? (map param-tipo type-params) type-args)
                                                                   (if (idS? fun)
                                                                       (param-tipo (last type-params))
                                                                       (if (equal?
                                                                            (typeof (funS-body fun) (get-context (funS-params fun) context))
                                                                            (get-las-type (funS-rType fun)))
                                                                           (get-las-type (funS-rType fun))
                                                                           (error (~a "type-app El valor de retorno no coincide " (typeof (funS-body fun) (get-context (funS-params fun) context)) "rr" (param-tipo (last type-params))))))
                                                                   (error (string-append "app: Type error:\nParameter's type doesn't match expected argument's type.\nGiven: " (~v type-fun)"\nExpected: (booleanT)")))]
      [else (error 'type-app "El número de parámetros y argumentos es distinto")])))

(define (get-context params context)
  (if (empty? params)
      context
      (match (car params)
        [(param p t) (get-context (cdr params) (gamma p t context))])))


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
               [else (erroR (cdr lst) "str" context)])]
    ["lts" (cond
               [(empty? lst) empty]
               [(not (listT? (typeof (car lst) context))) (car lst)]
               [else (erroR (cdr lst) "lts" context)])]))

;(require racket/trace)
;(trace typeof)
;(trace type-recs)
;(trace type-with)
;(trace get-las-type)
;(typeof (parse '{with [(x : number 8) (y : boolean #f)] {expt x y}}) (phi))
;(typeof (parse '{rec ([fac : (number -> number) {fun {(n : number)} : (number -> number) {if {zero? n} 1 {* n {fac ({- n 1})}}}}] [n : char #\5]) {fac (n)}}) (phi))
;(typeof (parse '{(fun ((n : number) (m : number)) : (number number -> number) (+ n m)) (9 3 8)}) (phi))
;(typeof (parse '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 #t}}) (phi))