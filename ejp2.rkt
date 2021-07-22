#lang plai

;; Ejercicios prácticos 2
;; @author Ramses López
;; @date June 2021

;; Tipo AE
(define-type AE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?) (args (listof AE?))])

;; Ejercicio
;; Procedimiento que se encarga de hacer una sustitución
;; subst :: AE symbol AE --> AE
(define (subst expr sub-id value)
  (match expr
    [(id i) (cond
              [(equal? i sub-id) value]
              [else expr])]
    [(num n) expr]
    [(op f lst) (op f (map (lambda (x) (subst x sub-id value)) lst))]))