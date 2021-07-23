#lang plai

;; Data-type que representa un caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value WAE?) (ds DefrdSub?)])

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value WAE?)])

;; Definición del tipo CFWAE
(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?) (args (listof WAE?))]
  [with (bindings (listof binding?)) (body WAE?)])
