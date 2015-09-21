#lang plai

(require "practica3-base.rkt")

; Seccion 1

;1.Dado el ritmo cardiaco de descanso y el máximo ritmo cardiaco de una persona se debe regresar la
;lista de zonas de frecuencia cardiaca.
(define (zones rest max )
    (list ;;aplicamos la formula para los estados
     (resting rest (+ rest(- (* (range rest max) 0.5) 1)))
     (warm-up (+ rest (* (range rest max) (+ 0.5 (* 0.1 0)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 1))) 1)))
     (fat-burning (+ rest (* (range rest max) (+ 0.5 (* 0.1 1)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 2))) 1)))
     (aerobic (+ rest (* (range rest max) (+ 0.5 (* 0.1 2)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 3))) 1)))
     (anaerobic (+ rest (* (range rest max) (+ 0.5 (* 0.1 3)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 4))) 1)))
     (maximum (+ rest (* (range rest max) (+ 0.5 (* 0.1 4)))) (+ rest  (* (range rest max) (+ 0.5 (* 0.1 5)))))
    ))

;; auxiliar para calcular el rango entre el ritmo en descanso y el máximo ritmo cardiaco.
(define (range rest max)
    (- max rest))

;Se define para ejemplos
(define my-zones (zones 50 180))

;2.Dada una zona y una lista regresa el tipo de dato correspondiente
(define (get-zone symbol lst)
  (cond
    [(eq? symbol 'resting) (list-ref lst 0)]
    [(eq? symbol 'warm-up) (list-ref lst 1)]
    [(eq? symbol 'fat-burning) (list-ref lst 2)]
    [(eq? symbol 'aerobic) (list-ref lst 3)]
    [(eq? symbol 'anaerobic) (list-ref lst 4)]
    [(eq? symbol 'maximum) (list-ref lst 5)]
    [else (error "El simbolo no esta en la lista")]))

;;3.Regresa una lista de zonas por cada fecuencia cardiaca en lst.
(define (bpm->zone lst zones)
  (cond
    [(or (empty? lst)(empty? zones)) '()]  
    [else (append
           (cond
             [(and (>= (car lst) (resting-low (first zones)))(<= (car lst) (resting-high (first zones))))  (list (first zones))]
             [else '()])
           (cond
             [(and (>= (car lst) (warm-up-low (second zones)))(<= (car lst) (warm-up-high (second zones)))) (list (second zones))]
             [else '()])
           (cond
             [(and (>= (car lst) (fat-burning-low (third zones)))(<= (car lst) (fat-burning-high (third zones))))  (list (third zones))]
             [else '()])
           (cond
             [(and (>= (car lst) (aerobic-low (fourth zones)))(<= (car lst) (aerobic-high (fourth zones)))) (list (fourth zones))]
             [else '()])
           (cond
             [(and (>= (car lst) (anaerobic-low (fifth zones)))(<= (car lst) (anaerobic-high (fifth zones)))) (list (fifth zones))]
             [else '()])
           (cond
             [(and (>= (car lst) (maximum-low (sixth zones)))(<= (car lst) (maximum-high (sixth zones))))  (list (sixth zones))]
             [else '()])
           (bpm->zone (cdr lst) zones))]))                                        
               
;;4.Dado una lista en la que cada elemento de la lista contiene: un tiempo en formato UNIX,
;;una lista con la latitud y longitud y finalmente el ritmo cardiaco. Como segundo parámetro se tiene una lista
;;de zonas cardiacas con lo que se tiene que regresar una lista de trackpoints que contengan la información
;;dada. 
(define (create-trackpoints lst zones)
  (cond 
    [(empty? lst) '()] ;; si la lista es vacia no construimos nada, regresamos la lista vacía
    [else (cons ;; sino construimos la lista acomodando cada parametro de la lista de entrada
        (trackpoint (GPS (first (second (car lst))) (second (second (car lst)))) (third (car lst)) (first (bpm->zone (list (third (car lst))) zones)) (first (car lst)))
        (create-trackpoints (cdr lst) zones) )]))

;5.Dada una lista trackpoints devuelve la distancia total
(define (total-distance tk)
  (cond
    [(not (list? tk)) error "No es una lista el primer elemento"]
    [(empty? tk) 0]
    [else (aux-total (car tk) (cdr tk))]))

(define (aux-total aux-gps gps-l)
    (cond
      [(empty? gps-l) 0]
      [else (+ (haversine (trackpoint-loc aux-gps) (trackpoint-loc (car gps-l))) (aux-total (car gps-l) (cdr gps-l)))]))

;6.average-hr
;Con una lista de trackpoints regresa el promedio del ritmo cardiaco.
(define (average-hr lsttk)
;Funcion auxiliar para sumar hr de una lista de trackpoints.
  (cond
    [(empty? lsttk) 0]
    [else(round(/ (suma lsttk) (length lsttk)))]))

;auxiliar que suma hr de una lista de trackpoints.
(define (suma lsttk)
  (cond
    [(empty? lsttk) 0]
    [else
     (define hr-aux (type-case Frame (car lsttk)
       [trackpoint (loc hr zone unix-time) hr]))
     (+ hr-aux (suma (cdr lsttk)))]))

;Ejemplos para crear tackpoints
(define sample (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints (create-trackpoints raw-data my-zones))
(define trackpoints1 (create-trackpoints (take raw-data 50) my-zones))
(define trackpoints2 (create-trackpoints (take raw-data 400) my-zones))
(define trackpoints3 (create-trackpoints (take raw-data 50) my-zones))
(define trackpoints4 (create-trackpoints (take raw-data 10) my-zones))
; Funcion haversine que devuelve la distancia entre dos coordenadas
; Auxiliar para total-distance
(define (haversine x y )
  (* (* 2 6371) (asin(sqrt(auxHaversine x y))) ))

;Aux para obtener el valor dentro de la raíz
(define (auxHaversine x y)
  (+ (* (sin(/ (- (aRadianes (GPS-lat y)) (aRadianes (GPS-lat x))) 2)) (sin(/ (- (aRadianes (GPS-lat y)) (aRadianes (GPS-lat x))) 2)))
     (* (* (sin(/ (- (aRadianes (GPS-long y)) (aRadianes (GPS-long x))) 2)) (sin(/ (- (aRadianes (GPS-long y)) (aRadianes (GPS-long x))) 2))) (* (cos(aRadianes (GPS-lat x))) (cos(aRadianes (GPS-lat y)))) )))

;Aux para transformar a Radianes (haversine)
(define (aRadianes l)
  (/(* l pi) 180))

;7.Dada una lista de trackpoints, regresar el máximo ritmo cardiaco, el resultado debe ser un entero
(define (max-hr lst)
  (cond 
    [(empty? lst) 0]
    [else (auxMax-hr  lst (trackpoint-hr (car lst)))])) ;; usa auxiliar

;;función auxiliar para max-hr recibe una lista de trackpoints y el máximo hasta ese momento
(define (auxMax-hr lst num) 
  (cond 
    [(empty? lst) num] ;;si la lista es vacia, terminamos de recorrerla y devolvemos el máximo encontrado
    [else
     (cond ;;comparamos el maximo hasta el momento con el siguiente ritmo cardiaco
       [(> num (trackpoint-hr (car lst))) (auxMax-hr (cdr lst) num)] ;;si el máximo es mayor a el ritmo cardiaco de la lista continuamos con el siguiente
       [else (auxMax-hr (cdr lst) (trackpoint-hr (car lst)))])])) ;; de lo contrario recursamos ahora con el nuevo máximo

;8.collapse-trackpoints
;Dada una lista de trackpoitns y un epsilon se agrupan los deltas consecutivos.
(define (collapse-trackpoints lsttk e)
  (cond
    [(empty? lsttk) '()]
    [(empty? (cdr lsttk)) (list (car lsttk))]
    [else         
     (append (if (and (= (trackpoint-hr (car lsttk)) (trackpoint-hr (cadr lsttk))) (< (haversine (trackpoint-loc (car lsttk)) (trackpoint-loc (cadr lsttk))) e)) '() (list (car lsttk))) (collapse-trackpoints (cdr lsttk) e))]))

(define sample-four (create-trackpoints (take raw-data 4) my-zones))
;Sección 2

;9.ninBT
;;Dado un árbol de tipo BTree determinar el número de nodos internos que tiene.
(define (ninBT tree)
  (cond
    [(or (EmptyBT? tree) (and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree)))) 0]    
    [else (+ 1 (ninBT (BNode-l tree)) (ninBT (BNode-r tree)))]))

;10.nlBT 
;;Dado un árbol de tipo BTree, determinar el número de hojas no vacías.
(define (nlBT tree)
  (cond
    [(EmptyBT? tree) 0] ;; caso base 
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1] ;; si ambos nodos hijos son vacíos entonces es hoja y es no vacía ya que no cayó en el caso anterior asi que la sumamos 
    [else (+ (nlBT (BNode-l tree)) (nlBT(BNode-r tree)) )])) ;; recursa sobre cada sub arbol y suma el resultado de estos

;11.Determina el número de nodos que tiene el árbol sin hojas vacias
(define (nnBT tree)
  (+ (nlBT tree) (ninBT tree)))

;12.mapBT
;Dado una función de aridad 1 y un árbol de tipo BTree, aplicar la función sobre todos los valores de los nodos del árbol.
(define (mapBT f tree)
  (cond
    [(EmptyBT? tree) (EmptyBT)]
    [else (bnn (mapBT f (BNode-l tree)) (f (BNode-e tree)) (mapBT f (BNode-r tree)))]))

;--------------------------------------RECORRIDOS EN ÁRBOLES-------------------------------------------

;1.preorderBT
(define (preorderBT tree)
  (cond
    [(EmptyBT? tree) '()]
    [else (append (list (BNode-e tree)) (preorderBT (BNode-l tree)) (preorderBT (BNode-r tree)))]))

;2.inorderBT
(define (inorderBT tree)
  (cond
    [(EmptyBT? tree) '()]
    [else (append (inorderBT (BNode-l tree)) (list (BNode-e tree)) (inorderBT (BNode-r tree)))]))

;3.posorderBT
(define (posorderBT tree)
  (cond
    [(EmptyBT? tree) '()]
    [else (append (posorderBT (BNode-l tree)) (posorderBT (BNode-r tree)) (list (BNode-e tree)))]))


;------------------------------------------------------------------------------------------------------
;Tests

;Test para zone
(test (zones 50 180)
      (list
       (resting 50 114.0)
       (warm-up 115.0 127.0)
       (fat-burning 128.0 140.0)
       (aerobic 141.0 153.0)
       (anaerobic 154.0 166.0)
       (maximum 167.0 180.0)))

(test (zones 0 0)
      (list 
       (resting 0 -1) 
       (warm-up 0 -1) 
       (fat-burning 0 -1) 
       (aerobic 0 -1) 
       (anaerobic 0 -1) 
       (maximum 0 0)))

(test (zones 10 30)
      (list
       (resting 10 19.0)
       (warm-up 20.0 21.0)
       (fat-burning 22.0 23.0)
       (aerobic 24.0 25.0)
       (anaerobic 26.0 27.0)
       (maximum 28.0 30.0)))

(test (zones 10 100)
      (list
       (resting 10 54.0)
       (warm-up 55.0 63.0)
       (fat-burning 64.0 72.0)
       (aerobic 73.0 81.0)
       (anaerobic 82.0 90.0)
       (maximum 91.0 100.0)))

(test (zones 50 150)
      (list
       (resting 50 99.0)
       (warm-up 100.0 109.0)
       (fat-burning 110.0 119.0)
       (aerobic 120.0 129.0)
       (anaerobic 130.0 139.0)
       (maximum 140.0 150.0)))

;Tests para get-zone
(test (get-zone 'warm-up my-zones)(warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones)(fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones)(aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones)(anaerobic 154.0 166.0));
(test (get-zone 'maximum my-zones)(maximum 167.0 180.0))

;Test bpm->zone
(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(50 60) my-zones) (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones) (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
(test (bpm->zone '(140 141) my-zones) (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))             
(test (bpm->zone '(60 120 150) my-zones) (list (resting 50 114.0) (warm-up 115.0 127.0) (aerobic 141.0 153.0)))

;Test create-trackpoints
(test (create-trackpoints (take raw-data 0) my-zones)
      '())

(test (create-trackpoints (take raw-data 1) my-zones)
      (list 
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))

(test (create-trackpoints (take raw-data 2) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))

(test (create-trackpoints (take raw-data 3) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)))

(test (create-trackpoints (take raw-data 4) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))

;Test total-distance
(test (total-distance '()) 0)
(test (total-distance (create-trackpoints (take raw-data 1) my-zones)) 0)
(test (total-distance trackpoints) 5.05510837344589)
(test (total-distance (create-trackpoints (take raw-data 100) my-zones)) 0.9509291243812747)
(test (total-distance trackpoints3) 0.46100326160491)

;Test average-hr
(test (average-hr empty) 0)
(test (average-hr sample) 134)
(test (average-hr trackpoints) 150)
(test (average-hr trackpoints1) 128)
(test (average-hr trackpoints2) 147)

;Test collapse-trackpoints
(test (collapse-trackpoints '() 22020022) '())
(test (collapse-trackpoints sample-four 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))

;Test average-hr
(test (max-hr empty) 0)
(test (max-hr sample) 147)
(test (max-hr trackpoints) 165)
(test (max-hr trackpoints1) 136)
(test (max-hr trackpoints2) 165)

;Test para ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (bns (bns (bns ebt "A" ebt) "B" (bns (bns ebt "C" ebt) "D" (bns ebt "E" ebt)))"F"(bns ebt "G" (bns (bns ebt "H" ebt) "I" ebt)))) 5)
(test (ninBT arb1) 0)
(test (ninBT arb4) 7)


;;tests para nlBT 
(test (nlBT arb1) 1)
(test (nlBT arb2) 2)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)
(test (nlBT (bnn arb4 5 arb4)) 16)

;Test nnBT
(test (nnBT (EmptyBT)) 0)
(test (nnBT arbol-base) 9)
(test (nnBT arb1) 1)
(test (nnBT arb2) 3)
(test (nnBT arb3) 7)

;;test para mapBT
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT sub1 (EmptyBT)) (EmptyBT))
(test (mapBT (lambda (x) (- 1 x)) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 0 (BNode < (EmptyBT) -1 (EmptyBT))))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))

;;Test preorderBT
(test (preorderBT (EmptyBT)) '())
(test (preorderBT arb1) '(1))
(test (preorderBT maxiarb) '(10 1 2 4 5 3 6 7 9 1 2 4 5 3 6 7 9))
(test (preorderBT arb4) '(4 3 2 1 1 2 1 1 3 2 1 1 2 1 1))
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))

;;Test inorderBT
(test (inorderBT (EmptyBT)) '())
(test (inorderBT arb1) '(1))
(test (inorderBT maxiarb) '(4 2 5 1 7 6 9 3 10 4 2 5 1 7 6 9 3))
(test (inorderBT arb2) '(1 2 1))
(test (inorderBT arb3) '(1 2 1 3 1 2 1))

;;Test posorderBT
(test (posorderBT (EmptyBT)) '())
(test (posorderBT arb1) '(1))
(test (posorderBT maxiarb) '(4 5 2 7 9 6 3 1 4 5 2 7 9 6 3 1 10))
(test (posorderBT arb4) '(1 1 2 1 1 2 3 1 1 2 1 1 2 3 4))
(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))