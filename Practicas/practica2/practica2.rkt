#lang plai

(define-type Array
  [MArray (length number?) (elements list?)])

(test (Array? (MArray 2 '("a" "b"))) #t)
(test (Array? (MArray 0 '())) #t)


(define-type MList
  [MEmpty]
  [MCons (value generic?) (next MList?)])

(define (generic? g) #t)

(test (MEmpty) (MEmpty))

(define-type NTree
  [TLEmpty]
  (NodeN (node number?) (children (listof NTree?))))

(test (TLEmpty) (TLEmpty))
(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))  (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))

(define-type Position
  [2D-Point (x number?) (y number?)])

(define-type Figure
  [Circle (center Position?) (radio number?)]
  [Square (vertex Position?) (length number?)]
  [Rectangle (vertex Position?) (heigth number?) (length number?)])

;Regresa una lista de tipo MList dada un MArray con todos los elementos de este último
(define (MArray2MList array)
  (cond
    [(empty? (MArray-elements array)) (MEmpty)]
    [else (MCons (car (MArray-elements array)) (MArray2MList (MArray (- (MArray-length array) 1) (cdr (MArray-elements array)))))]))

(define (setvalueA array posicion v)
  (cond
    [(> posicion (MArray-length array)) (error 'setvalueA "Out of bounds")]
    [else (MArray (MArray-length array) (auxSVA (MArray-elements array) posicion v))]))

;Funcion auxiliar de setvalueA que regresa una lista a partir de otra, cambiando
;solo el elemento en el inidce recibido por el valor.
(define (auxSVA array posicion v)
  (cond
    [(= posicion 0) (cons v (cdr array))]
    [else (cons (car array) (auxSVA (cdr array) (- posicion 1) v))]))
   

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))

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

(test (printML (MEmpty)) "[]")
(test (printML (MCons 4 (MEmpty))) "[4]")
(test (printML (MCons 7 (MCons 4 (MEmpty)))) "[7,4]")
(test (printML (MCons (MCons 4 (MCons 1 (MEmpty))) (MCons 20 (MEmpty)))) "[[4,1],20]")
(test (printML (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))  "[7,4,1]")

;Dadas dos MList, las concatena
(define (concatML lst1 lst2)
  (cond
  [(MEmpty? lst1) lst2]
  [(MEmpty? lst2) lst1]
  [else (MCons (MCons-value lst1)(concatML (MCons-next lst1) lst2))]))

(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MEmpty) (MEmpty)) (MEmpty))
(test (concatML (MCons 7 (MEmpty)) (MEmpty)) (MCons 7 (MEmpty)))
(test (concatML (MCons 3 (MEmpty)) (MEmpty)) (MCons 3 (MEmpty)))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))