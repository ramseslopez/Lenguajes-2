  #lang plai

;; Ejercicios prácticos 3
;; Ramses López
;; julio 2021

;; Definción del tipo Binding
(define-type Binding
  [binding (id symbol?) (value WAE?)])

;; Definición del tipo WAE
(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?) (args (listof WAE?))]
  [with (bindings (listof Binding?)) (body WAE?)])

;; Data-type del caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value WAE?) (ds DefrdSub?)])

;; Ejercicio 1
;; Procedimeinto que se encarga de buscar un id en un caché de sustituciones
;; lookup :: symbol DefrdSub --> WAE
(define (lookup var ds)
  (match ds
    [(mtSub) (error "Variable libre")]
    [(aSub n v c) (cond
                     [(equal? n var) v]
                     [else (lookup var c)])]))

;; Ejercicio 2
;; Procedimiento que hace una sustitución y realiza la operación correpondiente (si es el caso)
;; interp :: WAE DefrdSub --> WAE
(define (interp expr ds)
  (operate (subst expr ds)))

;; subst :: WAE DefrdSub --> WAE
(define (subst expr ds)
  (match expr
    [(id i) (lookup i ds)]
    [(num n) expr]
    [(op f xs) (op f (map (lambda (x) (subst x ds)) xs))]
    [(with ys body) (match ys
                                ['() (subst body ds)]
                                [(cons (binding a b) zs) (subst (with zs body) (aSub a b ds))])]))

;; operate :: WAE --> WAE
(define (operate xs)
  (match xs
    [(id i) xs]
    [(num n) xs]
    [(op g zs) (match g
                      [+ (num (apply + (map num-n zs)))]
                      [- (num (apply - (map num-n zs)))]
                      [* (num (apply * (map num-n zs)))]
                      [/ (num (apply / (map num-n zs)))]
                      [sub1 (cond
                                  [(equal? (length zs) 1) (num (sub1 (first zs)))]
                                  [else (num 1)])]
                      [add1 (cond
                                  [(equal? (length zs) 1) (num (add1 (first zs)))]
                                  [else "La cardinalidad de la lista es mayor a 1"])]
                      [modulo (cond
                                      [(equal? (length zs) 2) (num (modulo (first zs) (last zs)))]
                                      [else (error "La cardinalidad de la lista es mayor a 2")])]
                      [expt (cond
                                      [(equal? (length zs) 2) (num (expt (first zs) (last zs)))]
                                      [else (error "La cardinalidad de la lista es mayor a 2")])])]))






(operate (subst (with (list (binding 't (num 2)) (binding 'y (num 6))) (op + (list (id 't) (id 'y)))) (mtSub)))
