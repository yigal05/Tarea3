#lang racket
#|
- Fecha de publicación: 
- Hora de publicación:
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa recibe un numero del uno (1) al cinco (5) y devuelve si el numero es primo o no.
|#
(define (pareja n m c)
  (if (< n 9)
      (if (= (remainder n 3) 0)
          (begin
            (printf "~a ~a ~a\n" n m c)
            (pareja (+ n 1) (+ m 1) 1)
            )
          (begin
            (printf "~a ~a ~a\n" n m c)
            (pareja (+ n 1) m (+ c 1) )
            )
         
       )
      (printf "~a ~a ~a \n" n m c)
      
   )
 )

(pareja 1 1 1)