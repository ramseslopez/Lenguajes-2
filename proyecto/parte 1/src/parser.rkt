#lang plai

(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A ::=  <number>
;;        | <symbol>
;;        | listof(A)
;; parse :: A --> SCFWBAE
;; parse :: s-expression --> SCFWBAE
(define (parse sexp)
    (cond
        [(symbol? sexp) (idS sexp)]
        [(number? sexp) (numS sexp)]
        [(boolean? sexp) (boolS sexp)]
        [(char? sexp) (charS sexp)]
        [(string? sexp) (stringS sexp)]
        [(list? sexp) (parse-list sexp)]))


(define (parse-list sexp)
    (case (car sexp)
                  [(if) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 4) (error 'parse "Aridad incorrecta")]
                              [else (iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))])]
                  [(if0) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 4) (error 'parse "Aridad incorrecta")]
                              [else (iF0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))])]
                  [(+) (cond
                            [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                            [else (opS + (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(-) (cond
                            [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                            [else (opS - (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(*) (cond
                            [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                            [else (opS * (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(/) (cond
                            [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                            [else (opS / (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(add1) (cond
                                  [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                                  [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                                  [else (opS add1 (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(sub1) (cond
                                  [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                                  [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                                  [else (opS sub1 (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(modulo) (cond
                                      [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                                      [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                                      [else (opS modulo (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(expt) (cond
                                  [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                                  [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                                  [else (opS expt (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(<) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                              [else (opS < (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(<=) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                              [else (opS <= (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(>=) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                              [else (opS >= (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(>) (cond
                              [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                              [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                              [else (opS > (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [(zero?) (cond
                                    [(<= (length sexp) 1) (error 'parse "No hay argumentos suficientes")]
                                    [(> (length sexp) 2) (error 'parse "Aridad incorrecta")]
                                    [else (opS zero? (map (lambda (x) (parse x)) (cdr sexp)))])]
                  [else (listS (map (lambda (x) (parse x)) sexp))]|))
