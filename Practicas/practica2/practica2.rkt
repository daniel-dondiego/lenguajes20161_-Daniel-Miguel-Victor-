#lang plai

(define-type Array
  [MArray (leng number?) (elements list?)])

(test (Array? (MArray 2 '(1, 4))) #t)
(test (Array? (MArray 0 '())) #t)


(define-type MList
  [MEmpty]
  [MCons (value number?) (next MList?)])

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
