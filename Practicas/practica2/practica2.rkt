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
    [(>= posicion (MArray-length array)) (error 'setvalueA "Out of bounds")]
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
;Imprime las MList en un formato legible.
(define (printML mlist)  
  (cond
    [(MEmpty? mlist) "[]"]  
    [else (string-append "[" (string-append (separate mlist) "]"))]))

;auxiliar que pone una "," entre cada elemento.
(define (separate mlist)
  (define (comas mlist)
    (cond
      [(MEmpty? mlist) ""]
      [(MList? (MCons-value mlist))  (string-append (printML (MCons-value mlist)) (string-append "," (comas (MCons-next mlist))))]
      [else(string-append (~a (MCons-value mlist)) (string-append "," (comas (MCons-next mlist) )))]))
  (substring (comas mlist) 0 (-(string-length (comas mlist)) 1)))

;Dadas dos MList, las concatena
(define (concatML lst1 lst2)
  (cond
  [(MEmpty? lst1) lst2]
  [(MEmpty? lst2) lst1]
  [else (MCons (MCons-value lst1)(concatML (MCons-next lst1) lst2))]))

;Dada una  MList nos regresa su longitud
(define (lengthML mlist)
  (cond
  [(MEmpty? mlist) 0]
  [else (+(lengthML (MCons-next mlist)) 1)]))

;-------------------------------------------------------------------------------------------------------------
;Prubas Sección I

;Pruebas del tipo Array.
(test (Array? (MArray 0 '())) #t)
(test (Array? (MArray 1 '(8))) #t)
(test (Array? (MArray 4 '(1 2 3 4))) #t)
(test (Array? (MArray 4 '(1 2 3))) #t)
(test (Array? (MArray 7 '(1))) #t)

;Pruebas del tipo MList
(test (MEmpty) (MEmpty))
(test (MCons 1 (MEmpty)) (MCons 1 (MEmpty)))
(test (MCons 4 (MCons 3 (MCons 5 (MEmpty)))) 
      (MCons 4 (MCons 3 (MCons 5 (MEmpty)))))
(test (MCons 1 (MCons 7 (MCons 10 (MCons 6 (MEmpty))))) 
      (MCons 1 (MCons 7 (MCons 10 (MCons 6 (MEmpty))))))
(test (MCons 3 (MCons 2 (MEmpty))) (MCons 3 (MCons 2 (MEmpty))))

(test (TLEmpty) (TLEmpty))

(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))  (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))

;Pruebas Sección II

;Pruebas MArray2MList
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))

;Pruebas printML
(test (printML (MEmpty)) "[]")
(test (printML (MCons 4 (MEmpty))) "[4]")
(test (printML (MCons 7 (MCons 4 (MEmpty)))) "[7,4]")
(test (printML (MCons (MCons 4 (MCons 1 (MEmpty))) (MCons 20 (MEmpty)))) "[[4,1],20]")
(test (printML (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))  "[7,4,1]")

;Pruebas concatML
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MEmpty) (MEmpty)) (MEmpty))
(test (concatML (MCons 7 (MEmpty)) (MEmpty)) (MCons 7 (MEmpty)))
(test (concatML (MCons 3 (MEmpty)) (MEmpty)) (MCons 3 (MEmpty)))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))

;Pruebas lengthML
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty)))))) 4)
(test (lengthML (MCons 3 (MEmpty))) 1)
(test (lengthML (MCons 7 (MCons 4 (MCons 1 (MEmpty))))) 3)