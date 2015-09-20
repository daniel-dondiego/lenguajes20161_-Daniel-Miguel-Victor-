#lang plai

(require "practica3-base.rkt")

; Seccion 1

;1.Dado el ritmo cardiaco de descanso y el máximo ritmo cardiaco de una persona se debe regresar la
;lista de zonas de frecuencia cardiaca.
(define (zones rest max )
    (list
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
<<<<<<< HEAD
    [(empty? mz) empty]))

;;4.Dado una lista en la que cada elemento de la lista contiene: un tiempo en formato UNIX,
;;una lista con la latitud y longitud y finalmente el ritmo cardiaco. Como segundo parámetro se tiene una lista
;;de zonas cardiacas con lo que se tiene que regresar una lista de trackpoints que contengan la información
;;dada. 
(define (create-trackpoints lst zones)
  (if (empty? lst) empty
=======
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
               
;;4
; Función create-trackpoints 
(define (create-trackpoints l zones)
  (if (empty? l)
      empty
>>>>>>> f5aaa353155bd14133aca21933ce7adaacfe40ac
       (cons
        (trackpoint (GPS (first (second (car lst))) (second (second (car lst)))) (third (car lst)) (first (bpm->zone (list (third (car lst))) zones)) (first (car lst)))
        (create-trackpoints (cdr lst) zones) )))

;5.Dada una lista trackpoints devuelve la distancia total
;Victor
(define (total-distance trackpoints)
  (empty? trackpoints))

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


;8.Victor
(define (collapse-trackpoints trackpoints e)
  (cond
    [(empty? trackpoints) '()]
    [(empty? (cdr trackpoints)) (list (car trackpoints))]
    [else (empty? trackpoints)]))


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
;Victor
(define (nnBT h)
  (empty? h))



(define arbol-base (bns (bns (bns ebt "A" ebt) "B" (bns (bns ebt "C" ebt) "D" (bns ebt "E" ebt)))
"F"
(bns ebt "G" (bns (bns ebt "H" ebt) "I" ebt))))

;------------------------------------------------------------------------------------------------------
;Tests

;Zones
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

(test (zones 5 18)
      (list
       (resting 5 10.5)
       (warm-up 11.5 11.8)
       (fat-burning 12.8 13.1)
       (aerobic 14.1 14.4)
       (anaerobic 15.4 15.700000000000001)
       (maximum 16.700000000000003 18.0)))

(test (zones 10 20)
      (list
       (resting 10 14.0)
       (warm-up 15.0 15.0)
       (fat-burning 16.0 16.0)
       (aerobic 17.0 17.0)
       (anaerobic 18.0 18.0)
       (maximum 19.0 20.0)))

(test (zones 50 100)
      (list
       (resting 50 74.0)
       (warm-up 75.0 79.0)
       (fat-burning 80.0 84.0)
       (aerobic 85.0 89.0)
       (anaerobic 90.0 94.0)
       (maximum 95.0 100.0)))

;Tests para get-zone
(test (get-zone 'resting my-zones) 
      (resting 50 114.0))
(test (get-zone 'warm-up my-zones)
      (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones)
      (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones)
      (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones)
      (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones)
      (maximum 167.0 180.0))


;Test bpm->zone
(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(50 60) my-zones) (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones) (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))

;Test average-hr
(test (average-hr empty) 0)
(test (average-hr sample) 134)
(test (average-hr trackpoints) 150)
(test (average-hr trackpoints1) 128)
(test (average-hr trackpoints2) 147)

;Test para ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)

;;tests para nlBT 
(test (nlBT arb1) 1)
(test (nlBT arb2) 2)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)
(test (nlBT (bnn arb4 5 arb4)) 16)
