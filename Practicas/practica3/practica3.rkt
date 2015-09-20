#lang plai

(require "practica3-base.rkt")

; Seccion 1

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
(define (bpm->zone lst mz)
  (cond
    [(empty? mz) empty]))

;Dada una lista trackpoints devuelve la distancia total
;Victor
(define (total-distance trackpoints)
  (empty? trackpoints))

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


;Victor
(define (collapse-trackpoints trackpoints e)
  (cond
    [(empty? trackpoints) '()]
    [(empty? (cdr trackpoints)) (list (car trackpoints))]
    [else (empty? trackpoints)]))


;Sección 2


;;nlBT 
;;Dado un árbol de tipo BTree, determinar el número de hojas no vacías.
(define (nlBT tree)
  (cond
    [(EmptyBT? tree) 0] ;; caso base 
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1] ;; si ambos nodos hijos son vacíos entonces es hoja y es no vacía ya que no cayó en el caso anterior asi que la sumamos 
    [else (+ (nlBT (BNode-l tree)) (nlBT(BNode-r tree)) )])) ;; recursa sobre cada sub arbol y suma el resultado de estos

;;tests para nlBT 
(test (nlBT arb1) 1)
(test (nlBT arb2) 2)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)
(test (nlBT (bnn arb4 5 arb4)) 16)


;Determina el número de nodos que tiene el árbol sin hojas vacias
;Victor
(define (nnBT h)
  (empty? h))


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
       (maximum 167.0 180)))

;Tests para get-zone
(test (get-zone 'resting my-zones) 
      (resting 50 114.0))
(test (get-zone 'warm-up my-zones)
      (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones)
      (fat-burning my-zones))
(test (get-zone 'aerobic my-zones)
      (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones)
      (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones)
      (maximum 167.0 180))

;Test bpm->zone
(test (bpm->zone empty my-zones) '())

