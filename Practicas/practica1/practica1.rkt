#lang plai

(define (pow n m)
  (cond
    [(zero? m) 1]
    [(= m 1) n]
    [else (* n (pow n (- m 1)))]))

(define (length a_lst)
  (cond
    [(empty? a_lst) 0]
    [true (+ 1 ( length (cdr a_lst)))]))

(define (average lst)
  (cond
    [(empty? lst) 0]
    [else (/ (sumaAv lst) (length lst))]))

(define (sumaAv lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (sumaAv (cdr lst)))]))
 
(define (primos n)
  (cond
    [(< n 2) '()]
    [(= n 2) '(2)]
    [(esprimo n (- n 1)) (cons n (primos(- n 1)))]
    [else (primos (- n 1))]))
    
(define (esprimo n f)
  (cond
    [(= f 2) (> (remainder n f) 0)]
    [(= (remainder n f) 0) #f]
    [else (and #t (esprimo n (- f 1)))]))


(test 1 (pow 1 0))

(test 3 (length '(1 2 3)))

(test 9.5 (average '(10 9)))
