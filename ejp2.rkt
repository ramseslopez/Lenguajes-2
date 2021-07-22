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


(define (ids? e)
  (match e
    [(id i) #t]            
    [(num n) #f]))

(define (nids? e)
  (match e
    [(id i) #f]
    [(num n) #t]))

(define (idd e i v)
  (match e
    ['() '()]
    [(cons (id l) xs) (cond
                        [(equal? l i) v]
                        [else (idd xs i v)])]))