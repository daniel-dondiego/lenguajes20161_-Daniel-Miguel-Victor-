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

;;función zip que toma dos listas 
;;y devuelve pares de las i-ésimas entradas
;;verifica los casos en que alguna lista es vacía, donde devuelve la lista vacía y se detiene
;;en otro caso toma el primer elemento de la primera lista
;;y lo concatena con el primer elemento concatenado con la lista vacía, para que sea una lísta
;;y ese par se lo concatena a la llamada recursiva de la función con el resto de las listas
(define (zip a_lst b_lst)
  (cond 
    [(empty? a_lst) '()]
    [(empty? b_lst) '()]
    [else (cons (cons (car a_lst) (cons (car b_lst) '())) (zip (cdr a_lst) (cdr b_lst)))]))

;reduce, recibe dos listas
;Recibe una función binaria y una lista, evalua todos los elementos de la lista
;con la función
;Si la lista es vacía la regresa, si la lista contiene un solo elemento
;lo regresa, si no evalua la función con el primer elemento de la lista con la
;llamada recursiva sobre el resto de la lista.
(define (reduce f lst)  
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (car lst)]
    [else (f (car lst) (reduce f (cdr lst)))]))

;;mconcat
;;Concatena dos listas
;;Verifica si la primera lista es vacía, si lo es regresa la segunda,
;; si no toma la cabeza de la lista y la concatena con la llamada recursiva del
;; resto de la lista usando la función cons.
 (define (mconcat lst1 lst2)
   (cond
     [(empty? lst1) lst2]
     [else (cons (car lst1) (mconcat (cdr lst1) lst2))]))

;;funcion que aplica una funcion a cada elemento de una lista
(define (mmap f lst)
  (cond
    [(empty? lst) '()]
    [else (cons (f (car lst)) (mmap f (cdr lst)))]))

;;mfilter
;; Regresa una lista solo con los elementos que son verdad bajo un predicado
;; Si la lista es vacía se regresa la misma, si la cabeza de la lista devuelve true bajo
;; el predicado se regresa la cabeza concatenada con la llamda recursiva, en otro caso
;; no se toma en cuenta la cabeza y se recursa sobre el resto de la lista.
(define (mfilter p lst)
  (cond
    [(empty? lst) '()]
    [(p (car lst)) (mconcat (cons (car lst) '())(mfilter p (cdr lst)))]
    [else (mfilter p (cdr lst) )]))

;;any?
;; Regresa #t si algún elemento cumple con el predicado, #f en caso contrario,
;; verifica si la lista es vacia, si lo es regresa #f por que nada cumple el predicado.
;; Si encuentra alguno que cumpla regresa #t, en otro caso se recursa sobre el resto de la lista.
(define (any? p lst)
  (cond
    [(empty? lst) #f]
    [(p (car lst)) #t]
    [else (any? p (cdr lst))]))

;;every?
;;Regresa #t si todos los elementos de la lista cumplen el predicado
;; si la lista es vacía regresa #t, si no cumple el elemento actual de la lista regresa #f
;; si no sigues buscando alguno que no cumpla, si no lo encuentra y llega a la lista vacía regresa #t
(define (every? p  lst)
  (cond
    [(empty? lst) #t]
    [(not (p (car lst))) #f]
    [else (every? p (cdr lst))]))

;;----------------------------------------------------------------------------------------------------------
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

;;pruebas para la función zip
;;caso simple
(test '((1 3) (2 4)) (zip '(1 2) '(3 4)))
;;casos base
(test '() (zip '(1 2 3) '()))
(test '() (zip '() '(4 5 6)))
;;casos generales
(test '((8 3) (9 2)) (zip '(8 9) '(3 2 1 4)))
(test '((8 3) (9 4)) (zip '(8 9 1 2) '(3 4)))

;;prubas para reduce
;;caso simple
(test 55 (reduce + '(1 2 3 4 5 6 7 8 9 10)))
;;casos base
(test '() (reduce + '()))
(test 1 (reduce + '(1)))
;;casos generales
(test 0 (reduce - '(2 2 3 3 4 4)))
(test '((1 (4 7)) (2 (5 8)) (3 (6 9))) (reduce zip '((1 2 3) (4 5 6) (7 8 9))))

;;pruebas para mconcat
;;caso base
(test '() (mconcat '() '()))
(test '(1 2 3) (mconcat '(1 2 3) '()))
(test '(1 2 3) (mconcat '() '(1 2 3)))
;;caso general
(test '(1 2 3 4 5 6) (mconcat '(1 2 3) '(4 5 6)))
(test '(2 3 1 4 8 2 1) (mconcat '(2 3 1) '(4 8 2 1)))

;;pruebas para mmap
(test '(2 3 4 5) (mmap add1 '(1 2 3 4)))
(test '(1 4 7) (mmap car '((1 2 3) (4 5 6) (7 8 9))))
(test '((2 3) (5 6) (8 9))(mmap cdr '((1 2 3) (4 5 6) (7 8 9))))

;;pruebas para mfilter
;;caso base
(test '() (mfilter (lambda (x) (not (zero? x)) )'()))
;;casos generales
(test '(2 1 4) (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0)))
(test '((1 4 2) (2 4)) (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())))
(test '(2 4 6) (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6)))
(test '(3 6 9) (mfilter (lambda (n) (= (modulo n 3) 0)) '(1 2 3 4 5 6 7 8 9)))

;;pruebas para any?
;; casos base
(test #f (any? number? '()))
;;casos generales
(test #t (any? number? '(a b c d 1)))
(test #f (any? number? '(a b c d e)))
(test #f (any? symbol? '(1 2 3 4)))
(test #t (any? symbol? '(1 2 c 4)))

;;pruebas para every?
;;casos base
(test #t (every? number? '()))
;;casos generales
(test #t (every? number? '(1 2 3)))
(test #f (every? number? '(1 2 3 a)))
(test #f (every? number? '(a a a a a a)))
(test #t (every? symbol? '(a a a a a a a)))