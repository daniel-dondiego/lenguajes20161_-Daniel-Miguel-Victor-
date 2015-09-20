#lang plai

(require "practica3-base.rkt")

; Seccion 1

;Se define para ejemplos
(define my zones (zones 50 180))

;Dada una zona y una lista regresa el tipo de dato correspondiente
(define (get-zone symbol lst)
  (cond
    [(eq? symbol 'resting) (list-ref lst 0)]
    [(eq? symbol 'warm-up) (list-ref lst 1)]
    [(eq? symbol 'fat-burning) (list-ref lst 2)]
    [(eq? symbol 'aerobic) (list-ref lst 3)]
    [(eq? symbol 'anaerobic) (list-ref lst 4)]
    [(eq? symbol 'maximum) (list-ref lst 5)]
    [else (error "El simbolo no esta en la lista")]))

     
; Funcion haversine que devuelve la distancia entre dos coordenadas
(define (haversine x y )
  (* (* 2 6371) (asin(sqrt(auxHaversine x y))) ))

;Aux para obtener el valor dentro de la raíz
(define (auxHaversine x y)
  (+ (* (sin(/ (- (aRadianes (GPS-lat y)) (aRadianes (GPS-lat x))) 2)) (sin(/ (- (aRadianes (GPS-lat y)) (aRadianes (GPS-lat x))) 2)))
     (* (* (sin(/ (- (aRadianes (GPS-long y)) (aRadianes (GPS-long x))) 2)) (sin(/ (- (aRadianes (GPS-long y)) (aRadianes (GPS-long x))) 2))) (* (cos(aRadianes (GPS-lat x))) (cos(aRadianes (GPS-lat y)))) )))

;Aux para transformar a Radianes (haversine)
(define (aRadianes l)
  (/(* l pi) 180))




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
