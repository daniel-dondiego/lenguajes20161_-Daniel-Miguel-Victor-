#lang plai

;Sección I Tipos de datos recursivos y no recursivos.

;Definición de un tipo Array con un constructor
;MArray que tiene longitud y una lista.
(define-type Array
  [MArray (length number?) (elements list?)])

;Definición de un tipo List llamado MList con un 
;constructor MCons que recibe valores genericos.
(define-type MList
  [MEmpty]
  [MCons (value generic?) (next MList?)])

;Función auxiliar de MList para tener elementos
;genericos en la lista.
(define (generic? g) #t)

;Definición de un tipo Tree llamado NTree.
;Es un árbol n-ario de tipo NodeN y sus hojas seran
;nulas "TLEmpty".
(define-type NTree
  [TLEmpty]
  (NodeN (node generic?) (children (listof NTree?))))

;Definición de un tipo Position, que tiene un constructor 
;2D-Point. Este tipo representa un punto en el plano
;cartesiano.
(define-type Position
  [2D-Point (x number?) (y number?)])

;Definición de un tipo Figura. Tiene tres constructores:
;Circulo, Cuadrado y Rectangulo. 
(define-type Figure
  [Circle (center Position?) (radio number?)]
  [Square (vertex Position?) (length number?)]
  [Rectangle (vertex Position?) (heigth number?) (length number?)])


;Sección II. Funciones sobre Datos.

;Función que dado un array regresa otro array intercambiando
;en la posición del arreglo (position) el valor (v).
;Si position es igual o de mayor tamaño al tamaño del arreglo
;regresa un error.
(define (setvalueA array posicion v)
  (cond
    [(=> posicion (MArray-length array)) (error 'setvalueA "Out of bounds")]
    [else (MArray (MArray-length array) (auxSVA (MArray-elements array) posicion v))]))

;Funcion auxiliar de setvalueA que regresa una lista a 
;partir de otra, cambiando solo el elemento en el inidce 
;recibido por el valor.
(define (auxSVA array posicion v)
  (cond
    [(= posicion 0) (cons v (cdr array))]
    [else (cons (car array) (auxSVA (cdr array) (- posicion 1) v))]))
     

(define (MArray2MList array)
  (cond
    [(empty? (MArray-elements array)) (MEmpty)]
    [else (MCons (car (MArray-elements array)) (MArray2MList (MArray (MArray-length array) 
                                                                     (cdr  (MArray-elements array)) )))]))


;-------------------------------------------------------------------------------------------------------------

;Pruebas del tipo Array.
(test (Array? (MArray 4 '("a" "b"))) #t)
(test (Array? (MArray 0 '())) #t)

(test (MEmpty) (MEmpty))

(test (TLEmpty) (TLEmpty))
(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))  (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
