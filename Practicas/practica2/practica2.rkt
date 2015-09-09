#lang plai

(define-type Array
  [MArray (leng number?) (elements list?)])

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


(define (MArray2MList array)
  (cond
    [(empty? (MArray-elements array)) (MEmpty)]
    [else (MCons (car (MArray-elements array)) (MArray2MList (MArray (MArray-leng array) (cdr  (MArray-elements array)) )))]))

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
