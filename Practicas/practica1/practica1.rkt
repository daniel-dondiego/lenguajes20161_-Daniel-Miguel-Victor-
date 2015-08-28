#lang plai

;;función que calcula la potencia de un número n a la m
;;multiplicando m veces n.
(define (pow n m)
  (cond
    [(zero? m) 1]
    [(= m 1) n]
    [else (* n (pow n (- m 1)))]))

;;función auxiliar para calcular la longitud
;;de una lista, implementada sumando 1 al total 
;;cada que vemos un elemento y se detiene cuando haya 
;;recorrido todos.
(define (length a_lst)
  (cond
    [(empty? a_lst) 0]
    [true (+ 1 ( length (cdr a_lst)))]))

;;función auxiliar que calcula la suma de todos
;;los elementos de una lista, sumando al total el primer
;;elemento de cada sublista hasta recorrer toda.
(define (sumaAv lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (sumaAv (cdr lst)))]))

;;función que calcula el promedio de una lista de números
;;utilizando dos funciones auxiliares, una para sumar todos
;;los números de la lista y otra para calcular cuantos números
;;tiene la lista.
(define (average lst)
  (cond
    [(empty? lst) 0]
    [else (/ (sumaAv lst) (length lst))]))

(define (esprimo n f)
  (cond
    [(= f 2) (> (remainder n f) 0)]
    [(= (remainder n f) 0) #f]
    [else (and #t (esprimo n (- f 1)))]))

(define (primes n)
  (cond
    [(< n 2) '()]
    [(= n 2) '(2)]
    [(esprimo n (- n 1)) (cons n (primes(- n 1)))]
    [else (primes (- n 1))]))

;;pruebas para la función pow
;;casos base
(test 1 (pow 3 0))
(test 4 (pow 4 1))
;;casos generales
(test 4 (pow 2 2))
(test 27 (pow 3 3))
(test 256 (pow 4 4))

;;pruebas para la función average
(test 10 (average '(10 10 10 10 10)))
(test 9.5 (average '(10 9 10 9 10 9)))
(test 7.5 (average '(10 5)))
(test 3 (average '(1 2 3 4 5)))
(test 4 (average '(1 10 1 1 10 1)))

;;pruebas para la función primes
;;casos base
(test '() (primes 1))
(test '(2) (primes 2))
;;casos generales
(test '(7 5 3 2) (primes 10))
(test '(11 7 5 3 2) (primes 11))
(test '(19 17 13 11 7 5 3 2) (primes 20))

