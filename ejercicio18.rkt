#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 4:49
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa genera una terna de enteros.
|#
(define (Terna primerFila segundaFila tercerFila)
  (if (< primerFila 9)
      (if (= (remainder primerFila 3) 0)
          (begin
            (printf "~a ~a ~a\n" primerFila segundaFila tercerFila)
            (Terna (+ primerFila 1) (+ segundaFila 1) 1)
            )
          ;else
          (begin
            (printf "~a ~a ~a\n" primerFila segundaFila tercerFila)
            (Terna (+ primerFila 1) segundaFila (+ tercerFila 1) )
            )
         
       );endif
      (printf "~a ~a ~a \n" primerFila segundaFila tercerFila)
      
   );endif
 )

(Terna 1 1 1)