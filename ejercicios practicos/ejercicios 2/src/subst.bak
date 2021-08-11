#lang plai

(require (file "./grammars.rkt"))

;; Ejercicio 1
;; Procedimiento que se encarga de hacer una sustitución
;; subst :: AE symbol AE --> AE
(define (subst expr sub-id value)
  (match expr
      [(id i) (cond
                    [(equal? i sub-id) value]
                    [else expr])]
      [(num n) expr]
      [(op f lst) (op f (map (lambda (x) (subst x sub-id value)) lst))]))
