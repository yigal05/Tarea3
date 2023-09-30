#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 11:32
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime una V conformada por Zs.
|#
(define (Imprimir n)
  (if (= n 10)
      (printf "~a\n" (string-append (make-string 9 #\space) "Z") )
      ;else
      (begin
        (printf "~a\n" (string-append (make-string (- n 1) #\space) "Z"  (make-string (- 19 (* 2 n)) #\space) "Z" ) ) 
        (Imprimir (+ n 1))
       )
   );endif
 )

(Imprimir 1)