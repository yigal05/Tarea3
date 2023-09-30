#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 5 :26
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa genera una pareja de enteros.
|#

(define (pareja n m)
  (if (< n 9)
      (if (= (remainder n 2) 0)         
          (begin
            (printf "~a ~a \n" n m)
            (pareja (+ n 1) m)
            )
          ;else
          (begin
            (printf "~a ~a \n" n m)
            (pareja (+ n 1) (+ m 1))
            )
       );endif
      ;else
      (printf "~a ~a \n" n m)
      
   );endif
 )

(pareja 0 1)