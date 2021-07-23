#lang plai

;; Definición del tipo AE
(define-type AE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?) (args (listof AE?))])
