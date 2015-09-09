#lang plai

(define-type Array
  [MArray (leng number?) (elements list?)])

(test (Array? (MArray 2 '(1, 4))) #t)
(test (Array? (MArray 0 '())) #t)


(define-type MList
  [MEmpty]
  [MCons (value number?) (next MList?)])
